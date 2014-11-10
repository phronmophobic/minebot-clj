(ns minecraft-clj.scratch
)


(use 'minebot-clj.cell)


(def scratch-env (atom (minebot_clj.cell.Environment. {} {} {})))
(defmacro with-scratch
  ([sym]
     `(swap! scratch-env set-form (quote ~sym) ~sym))
  ([sym body]
     `(swap! scratch-env set-form (quote ~sym) (quote ~body))))

(with-scratch env-vals
  (fn []
    (:vals @scratch-env)))
(swap! scratch-env set-val 'set-val
       #(swap!
         scratch-env set-val %1 %2))
(with-scratch find-child
  (fn [ref name]
    (.findChild (.window ref) nil name)))
(with-scratch msg)
(with-scratch set-form
  #(swap! scratch-env set-form %1 %2))
(with-scratch shake
  (fn [name]
    (swap! scratch-env shake name)))


(with-scratch show-ui)
(with-scratch make-ui!
  (apply show-ui ui))
(with-scratch normalize-ui)

(with-scratch draw-shapes)

(with-scratch selected-block-id nil)
(with-scratch scratchblock)
(with-scratch scratchgroup)





(with-scratch v-)
(with-scratch v+)
(with-scratch vdist)
(with-scratch box)

(swap! scratch-env set-val 'draw-calls
       draw-calls)
(swap! scratch-env set-val 'ScratchBlock minebot_clj.cell.ScratchBlock)
(swap! scratch-env set-val 'IDraw IDraw)
(swap! scratch-env set-val 'ScratchGroup minebot_clj.cell.ScratchGroup)
(swap! scratch-env set-val 'zpeek zpeek)
(swap! scratch-env set-val 'seqz seqz)
(swap! scratch-env set-val 'zget zget)
(swap! scratch-env set-val 'zip zip)
(swap! scratch-env set-val 'zremove zremove)
(swap! scratch-env set-val 'zedit zedit)
(swap! scratch-env set-val 'znth znth)
(swap! scratch-env set-val 'unzip unzip)

(swap! scratch-env set-val 'zzroot zzroot)
(swap! scratch-env set-val 'zzremove zzremove)
(swap! scratch-env set-val 'zzseq zzseq)
(swap! scratch-env set-val 'zzedit zzedit)
(swap! scratch-env set-val 'zzget zzget)
(swap! scratch-env set-val 'zzinsert-left zzinsert-left)
(swap! scratch-env set-val 'zzinsert-right zzinsert-right)

(with-scratch selected-block nil)
(with-scratch highlight nil)

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
(swap! scratch-env set-val
       'find-scratch-block find-scratch-block)

(swap! scratch-env set-val
       'find-scratch-block find-scratch-block)

(swap! scratch-env set-val
       'size size)



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


(swap!
 scratch-env set-val
 'find-clicked
 find-clicked)

(swap!
 scratch-env set-val
 'flatten-draws
 flatten-draws)

(swap! scratch-env set-val 'blocks
  (list
   (scratchgroup [50 50] [(scratchblock "alone")])
   (scratchgroup [200 50] [(scratchblock "hi" [(scratchblock "a")
                                               (scratchblock "aa")
                                               (scratchblock "AA")
                                               (scratchblock "AA")
                                               (scratchblock "AA")
                                               (scratchblock "b"
                                                             [(scratchblock "money")])])])))


(with-scratch ui
  [:scratch2
   [:QVBoxWidget {:minimumSizeHint [400 800]
                  :pos [750 20]}
   [:QLabel {:text (with-out-str
                     (clojure.pprint/pprint blocks))}]
    [:QScratchArea {:background-color [200 200 255]
                    :mouseMoved (fn [this pos]
                                  (try
                                    (let [draws (flatten-draws blocks nil)
                                          points (->> draws
                                                      (filter #(instance? ScratchBlock (:draws %)))
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
                                      
                                      (if (< (vdist pos closest) 75)
                                        (set-val 'highlight [:drawRect rx ry 10 10])
                                        (set-val 'highlight nil)))
                                   (catch Exception e
                                     (msg e)))
                                  
                                  (when selected-block
                                    (set-val 'selected-block
                                             (assoc selected-block :pos pos))))
                    :mouseReleased (fn [this pos]
                                     (set-val 'highlight nil)
                                     
                                     (when selected-block
                                       (set-val 'selected-block nil)
                                       (let [block-draws (filter #(instance? ScratchBlock (:draws %))
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
                                               (set-val 'blocks
                                                        (-> block
                                                            (insert-op
                                                             (first (:blocks selected-block)))
                                                            zzroot)))
                                             (catch Exception e
                                               (msg e)))
                                           (set-val 'blocks
                                                    (conj blocks selected-block)))))
                                     )
                    :mousePressed (fn [this [x y]]
                                    (try
                                     (let [clicked
                                           (->> (find-clicked [x y]
                                                              (zzseq blocks))
                                                (filter #(instance? ScratchBlock (first %)))
                                                first
                                                first)]
                                       
                                       (try
                                         (if clicked
                                           (let [blocks (-> clicked zzremove zzroot
                                                            (zzedit #(remove (comp empty? :blocks) %)))
                                                 selected-block (scratchgroup [x y] [(vary-meta clicked
                                                                                                dissoc :path)])]

                                             (set-val 'blocks blocks)
                                             (set-val 'selected-block selected-block)
                                             )
                                           (do
                                             (set-val 'selected-block nil)))
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




