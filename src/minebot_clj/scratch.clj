(ns minebot-clj.scratch
  (:require [minebot-clj.cell :refer [ref-environment set-form shake! show-ui set-ref  msg


v-
v+
vdist
box
draw-calls
->ScratchBlock
IDraw
->ScratchGroup
scratchgroup
messages
                                      scratchblock
                                      draw-shapes
                                      size

                                      ]]
            [net.cgrand.enlive-html :as enlive]
            [minebot-clj.ui :as ui :refer [connect]]
            [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        ] :as async])
  (:use [minebot-clj.zipper :exclude [zseq] :as zip])
  (:use [minebot-clj.evaluable])
  (:import com.trolltech.qt.core.QCoreApplication)
  (:import minebot_clj.cell.ScratchBlock)
  (:import minebot_clj.cell.ScratchGroup)
  (:gen-class))



(def renv (atom (ref-environment)))
(let [ns *ns*]
  (defmacro r! [name form]
    `(do
       (let [form# (quote ~form)
             evaluable# (if (evaluable? form#)
                          form#
                          (->ClojureEvaluable ~ns form#
                                              (into {}
                                                    [~@(for [[k _] &env]
                                                         [(list 'quote k)
                                                          k])])))
             deps# (-> (dependencies evaluable#)
                       (->> (remove (fn [name#]
                                      (when-let [var# (ns-resolve ~ns name#)]
                                        (-> var# meta :reactive? not)))))
                       set 
                       (disj ~name))]
         (swap! renv set-form (quote ~name) evaluable# deps#)
         (dosync
          (shake! (deref renv) (quote ~name)))
         (when (instance? clojure.lang.IDeref ~name)
           (deref ~name))))))
(defmacro defr [name form]
  `(do
     (let [ref# (ref nil)]
      (defonce ~(vary-meta name assoc :reactive? true) ref#)
      (swap! renv set-ref (quote ~name) ref#))
     (r! ~name ~form)))




(defr selected-block nil)
(defr highlight nil)

(defn find-scratch-block [zm block]
  (when zm
    (let [obj (zpeek zm)]
      (cond
       (identical? obj block)
       zm

       (seq? obj)
       (first (keep #(find-scratch-block % block) (seqz zm)))

       (instance? minebot_clj.cell.ScratchGroup obj)
       (find-scratch-block (zget zm :block) block)

       (instance? minebot_clj.cell.ScratchBlock obj)
       (first (keep #(find-scratch-block % block) (seqz (zget zm :subblocks))))))))



(defn flatten-draws [draws context]
  (cond
   (vector? draws)
   (let [call draws
         call-type (first call)]
     (let [[_ dpos & calls] call]
       (if (= :translate call-type)
         (flatten-draws calls (conj context [:translate dpos]))
         [call])))

   (satisfies? IDraw draws)
   (concat [{:draws draws
             :context context}]
           (flatten-draws (draw-calls draws) context))

   (seq? draws)
   (mapcat #(flatten-draws % context) draws)))

(defn find-clicked
  ([pos draws]
     (find-clicked pos draws nil))
  ([[x y :as pos] draws path]
     (cond
      (satisfies? IDraw draws)
      (let [[w h] (box draws)
            [x y] (if (contains? draws :pos)
                    (v- pos (:pos draws))
                    pos)]
        (if (and (>= x 0)
                 (>= y 0)
                 (<= x w)
                 (<= y h))
          (let []
            (concat (find-clicked pos (draw-calls draws) (conj path draws))
                    (list [draws path])))))

      (seq? draws)
      (mapcat #(find-clicked pos % path) draws)

      (vector? draws)
      (let [call draws
            call-type (first call)]
        (if (= :translate call-type)
          (let [[_ dpos & calls] call]
            (concat (find-clicked (v- [x y]
                                      dpos)
                                  calls
                                  (conj path call)))))))))

(defn draw-tree-branch? [obj]
  (or (satisfies? IDraw obj)
      (seq? obj)
      (and (vector? obj)
           (= :translate (first obj)))))

(defn draw-tree-children [obj]

  (cond
   (satisfies? IDraw obj)
   (list (draw-calls obj))

   (and (vector? obj)
        (= :translate (first obj)))
   (nthrest obj 2)

   (seq? obj)
   (seq obj)

   ))

(defn draw-tree-seq [obj]
  (tree-seq
   draw-tree-branch?
   draw-tree-children
   obj))


(defr blocks
  (list
   (scratchgroup [50 50] [(scratchblock "alone")])
   (scratchgroup [200 50] [(scratchblock "hi" [(scratchblock "a")
                                               (scratchblock "aa")
                                               (scratchblock "AA")
                                               (scratchblock "AA")
                                               (scratchblock "AA")
                                               (scratchblock "b"
                                                             [(scratchblock "money")])])])))


(defr ui
  [:scratch2
   [:QVBoxWidget {:minimumSizeHint [400 800]
                  :pos [750 20]}
   [:QLabel {:text (with-out-str
                     (clojure.pprint/pprint blocks))}]
    [:QScratchArea {:background-color [200 200 255]
                    :mouseMoved (fn [this pos]
                                  
                                  (let [draws (flatten-draws blocks nil)
                                        points (->> draws
                                                    (filter #(instance? minebot_clj.cell.ScratchBlock (:draws %)))
                                                    (mapcat (fn [m]
                                                              (let [draws (:draws m)
                                                                    context (:context m)
                                                                    pos (->> context
                                                                             (map second)
                                                                             (apply v+))
                                                                    [width height] (size draws)]
                                                                [pos
                                                                 (v+ pos [0 height])
                                                                 (v+ pos [(int (/ width 2))
                                                                          (int (/ height 2))])]))))
                                        [rx ry :as closest]
                                        (first (->> points
                                                    (sort-by #(vdist pos %))))]
                                    (dosync
                                     (if (< (vdist pos closest) 75)
                                       (r! highlight [:drawRect rx ry 10 10])
                                       (r! highlight nil)))
                                    (when selected-block
                                      (r! selected-block
                                          (assoc selected-block :pos pos)))))
                    :mouseReleased (fn [this pos]
                                     (dosync
                                      (r! highlight nil)
                                      
                                      (when selected-block
                                        (r! selected-block nil)
                                        (let [block-draws (filter #(instance? minebot_clj.cell.ScratchBlock (:draws %))
                                                                  (flatten-draws (zzseq blocks) nil))
                                              points (->> block-draws
                                                          (mapcat (fn [m]
                                                                    (let [draws (:draws m)
                                                                          context (:context m)
                                                                          pos (->> context
                                                                                   (map second)
                                                                                   (apply v+))
                                                                          [width height] (size draws)]
                                                                      [[draws pos :above]
                                                                       [draws (v+ pos [0 height]) :below]
                                                                       [draws (v+ pos [(int (/ width 2))
                                                                                       (int (/ height 2))]) :inside]]))))
                                              [block point relative]
                                              (first (->> points
                                                          (sort-by #(vdist pos (second %)))))]
                                          (if (< (vdist pos point) 75)
                                            (try
                                              (let [insert-op (case relative
                                                                :above zzinsert-left
                                                                :below zzinsert-right
                                                                :inside #(zzedit %1 update-in [:subblocks] conj %2))]
                                                (r! blocks
                                                    (-> block
                                                        (insert-op
                                                         (first (:blocks selected-block)))
                                                        zzroot)))
                                              (catch Exception e
                                                (msg e)))
                                            (r! blocks
                                                (conj blocks selected-block))))))
                                     )
                    :mousePressed (fn [this [x y]]
                                    (try
                                     (let [clicked
                                           (->> (find-clicked [x y]
                                                              (zzseq blocks))
                                                (filter #(instance? minebot_clj.cell.ScratchBlock (first %)))
                                                first
                                                first)]
                                       
                                       (try
                                         (if clicked
                                           (let [blocks (-> clicked zzremove zzroot
                                                            (zzedit #(remove (comp empty? :blocks) %)))
                                                 selected-block (scratchgroup [x y] [(vary-meta clicked
                                                                                                dissoc :path)])]

                                             (r! blocks blocks)
                                             (r! selected-block selected-block)
                                             )
                                           (do
                                             (r! selected-block nil)))
                                         ))
                                     (catch Exception e
                                           (msg e))))
                    :onDraw (fn [this painter]
                              (draw-shapes painter blocks)
                              (when highlight
                                (draw-shapes painter highlight))
                              (when selected-block
                                (draw-shapes painter selected-block)))
                    :blocks
                    blocks}]]])



(defr ui! (show-ui :scratch3 (second ui)))
