(ns minebot-clj.drawing
  (:require [minebot-clj.penumbra :as pen
             :refer [IComponent
                     defcomponent
                     IBounds
                     IMouseDown
                     IMouseMove
                     IMouseDrag
                     IMouseUp
                     IKeyPress
                     IDraw
                     IChildren
                     IBounds
                     IOrigin
                     rectangle
                     draw
                     center
                     label
                     find-offset
                     vertical-layout
                     horizontal-layout
                     bounds
                     origin
                     make-cid
                     spacer
                     button
                     move
                     key-handler
                     scroll-area
                     text-input
                     start
                     ]]
            [minebot-clj.environment :as env :refer [defr r! rv! ru! r? with-renv]]
            [minebot-clj.zipper :as z])
  (:require [minebot-clj.evaluable :refer
             [evaluate
              evaluable?
              ->ClojureEvaluable
              constant-evaluable
              dependencies]])
  (:require [clojure.core.async :refer [go >! >!! <! <!!  put! chan
                                        close! timeout
                                        alts! alts!! thread
                                        ] :as async])
  (:require [clj-pdf.core :as pdf])
  (:require [environ.core :refer [env]])
  (:require [taoensso.faraday :as far])
  (:require [clojure.algo.graph :as graph])
  )


(def messages (atom []))
(def out (atom *out*))
(defn msg [& s]
  (binding [*out* @out]
    (apply println s))
  (swap! messages (fn [xs s]
                    (let [xs (conj xs s)]
                      (if (> (count xs) 30)
                        (subvec xs 1)
                        xs)))
         s))

;; (def shapes [])


(defmacro goe [ & body]
  `(go
    (try
      ~@body
      (catch java.lang.Exception e#
        (binding [*out* @out]
          (msg "error" (with-out-str
                         (clojure.stacktrace/print-stack-trace e#))))))))

(defmacro with-tap [[tap mult] & body]
  `(let [mult# ~mult]
     (async/tap mult# ~tap)
     (let [val# (do ~@body)]
       (async/untap mult# ~tap)
       val#)))

(defn key-thing [key-fn]
  (reify
    IComponent
    (cid [this]
      (pen/make-cid "key-thing"))
    IDraw
    (draw [this state]
      nil)
    IKeyPress
    (-key-press [this key]
      (key-fn key))))

(defn mouse-thing [mouse-fn]
  (reify
    IComponent
    (cid [this]
      (pen/make-cid "mouse-thing"))
    IDraw
    (draw [this state]
      nil)
    IMouseDrag
    (-mouse-drag [this [mx my]]
      (mouse-fn :drag mx my))
    IMouseUp
    (-mouse-up [this [mx my]]
      (mouse-fn :up mx my))
    IMouseMove
    (-mouse-move [this [mx my]]
      (mouse-fn :move mx my))
    IMouseDown
    (-mouse-down [this [mx my]]
      (mouse-fn :down mx my))))

(defrecord Point [x y]
  clojure.lang.Indexed
  (nth [point i]
    (case i
      0 x
      1 y))
  (nth [point i not-found]
    (case i
      0 x
      1 y)))

;; (defrecord Line [p1 p2])

(defrecord Rectangle [p1 p2])

(defrecord Circle [p1 p2])


;; (def mouse-ch (chan 10))
;; (def mouse-mult (async/mult mouse-ch))



#_(defn next-matching [mult xform]
  (let [ch (chan (async/dropping-buffer 1) xform)]
    (goe
     (with-tap [ch mouse-mult]
       (<! ch)))))

#_(defn next-mouse! [mtype]
  (next-matching mouse-mult
                 (comp (filter #(= mtype (first %)))
                       (map #(->> %
                                  (drop 1)
                                  vec)))))

#_(defn mouse-down! []
  (next-mouse! :down))

#_(defn mouse-up! []
  (next-mouse! :up))

#_(defn mouse-move! []
  (next-mouse! :move))

#_(defn mouse-drag! []
  (next-mouse! :drag))

#_(defr running false)
#_(defn stop! []
  (rv! running false))

;; (defr lines
;;   [])
;; (defr circles [])
;; (defr rectangles [])


;; (defr points {})

(defn sq [x]
  (* x x))

(defn dist [[x y] [u v]]
  (java.lang.Math/sqrt
   (+ (sq (- u x))
      (sq (- v y)))))



#_(defcell mode-selector [mouse-input keyboard-inputs]
  (let [current-mode (ref (default-mode))]
    (go
     (let [[val port] (alts! [mouse-input keyboard-inputs])
           change-mode? 
           cancel? (or change-mode?
                       )]
       (cond
        (or (and selected-mode?
                 different-mode?))
        (ref-set current-mode new-mode)

        cancel-key?
        (ref-set current-mode (restart @current-mode))

        (forward @current-mode [val port]))))))


(defrecord Cell [inputs formula]
  clojure.lang.IDeref
  (deref [cell]
    (apply formula (map deref inputs))))

(defn make-cell [inputs formula]
  (let [memo-formula (memoize formula)]
    (Cell. inputs memo-formula)))


(defprotocol ICell
  (cell-inputs [cell]))

(deftype CellList [inputs]
  clojure.lang.IDeref
  (deref [cell]
    (mapv deref inputs))

  clojure.lang.ISeq
  (next [cell]
    (if-let [n (next inputs)]
      (CellList. n)))
  (first [cell]
    (first inputs))
  (more [cell]
    (if-let [n (next cell)]
      n
      '()))

  (cons [cell input]
    (CellList. (conj inputs input)))

  (empty [cell]
    (CellList. []))
  (equiv [cell obj] (= inputs obj))

  clojure.lang.Indexed
  (nth [_ i]
    (nth inputs i))

  (nth [_ i not-found]
    (nth inputs i not-found))

  clojure.lang.Counted
  (count [cell]
      (count inputs))

  clojure.lang.Associative
  (containsKey [_ k]
    (.containsKey inputs k))
  (entryAt [_ k]
    (.entryAt inputs k))
  (assoc [_ k v]
    (CellList. (.assoc inputs k v)))

  java.lang.Iterable
  ;; todo - something more efficient
  (iterator [cell]
    (when-not (nil? inputs)
      (.iterator inputs)))

  ICell
  (cell-inputs [cell]
    inputs)

  clojure.lang.Seqable
  (seq [cell]
    (when (seq inputs)
      cell)))

(defmethod print-method CellList
  [s w]
  ((get-method print-method clojure.lang.IDeref) s w))

(defmethod print-method Cell
  [s w]
  ((get-method print-method clojure.lang.IDeref) s w))

;; (defmethod print-method CellList [v ^java.io.Writer w]
;;   (.write w "CellList"))

;; #_(defmethod print-method MyRecord [x ^java.io.Writer writer]
;;   (print-method (:a x) writer))

(defmethod print-dup CellList [x ^java.io.Writer writer]
  (.write writer "CellList"))

(defmethod print-dup Cell [x ^java.io.Writer writer]
  (.write writer "Cell"))

(prefer-method clojure.pprint/simple-dispatch clojure.lang.IDeref clojure.lang.ISeq)

(defn cell-list [& cells]
  (CellList. (vec cells)))

(defn v+ [& vs]
  (apply mapv + vs))

(defn v- [& vs]
  (apply mapv - vs))

(defn vmax [& vs]
  (apply mapv max vs))

(defn vmin [& vs]
  (apply mapv min vs))


(def p1 (ref nil))
(def temp-p2 (ref nil))
(def lines (ref (cell-list)))

(defr steps [])
(defr p1 nil )
(defr p1-str "")
(defr m1 nil)
(defr mode :move)

(defn hydrate-prog [steps]
  `(let [~'canvas (DynamicCanvas. [])
         ~@(apply
            concat
            (for [step steps]
              ['canvas `(-> ~'canvas ~step)]))]
     ~'canvas))

(defn hydrate-steps [steps]
  (eval (hydrate-prog steps)))



(defrecord SnapPoint [name val])

;; draw shape from to
;; move snap point from to
;; scale from point by Amount
;; rotate around point by Amount
(defprotocol IDynamicCanvas
  (add-line [canvas line])
  (move-line [canvas line-id delta])
;;  (move-line [canvas line-id dist])
;;  (scale-line [canvas line-id])
  )

(defprotocol ISnapPoints
   (snap-points [obj]))


(defmulti snap-point (fn [step snap-type]
                       [(type step) snap-type]))

(defrecord Line [start end]
  IComponent
  (cid [this])

  ISnapPoints
  (snap-points [this]
    [:start :end])

  IDraw
  (draw [this state]
    (let [p1 (:start this)
          p2 (:end this)]
      (draw (pen/path p1
                      p2)
            state))
    ))


(defrecord DynamicCanvas [lines]
  IDynamicCanvas
  (add-line [canvas line]
    (DynamicCanvas. (conj lines line)))
  (move-line [canvas line-id delta]
    (let [old-line (get lines line-id)
          new-line (Line. (v+ delta
                              (:start old-line))
                          (v+ delta
                              (:end old-line)))]
     (DynamicCanvas. (assoc lines line-id new-line))))
  #_(replace-line [canvas line-id line]
    (DynamicCanvas. (assoc lines line-id line))))




(defmethod snap-point [Line :start] [step type]
  (:start step))
(defmethod snap-point [Line :end] [step type]
  (:end step))




(defmethod snap-point [Rectangle :bottom-left] [step type]
  (vmin (:start step) (:end step)))
(defmethod snap-point [Rectangle :bottom-right] [step type]
  (let [pts [(:start step)
             (:end step)] ]
    [(apply max (map first pts))
     (apply min (map second pts))]))
(defmethod snap-point [Rectangle :top-left] [step type]
  (let [pts [(:start step)
             (:end step)] ]
    [(apply min (map first pts))
     (apply max (map second pts))]))
(defmethod snap-point [Rectangle :top-right] [step type]
  (vmax (:start step) (:end step)))

(defrecord Rectangle [start end]
  IComponent

  ISnapPoints
  (snap-points [this]
    [:bottom-left
     :bottom-right
     :top-left
     :top-right])

  IDraw
  (draw [this state]
    (let [[x y] (vmin start end)
          [width height] (v- (vmax start end) [x y])]
      (draw (pen/move x y
                      (pen/rectangle width height))
            state))
    ))



(comment
 (Line. start end))
;; creates line
;; has snap points

(comment
  (Rectangle. start end)
  (Move. shape anchor point)
  (ScaleX. shape anchor scalex)
  (ScaleY. shape anchor scaley)
  (ScaleXY. shape anchor scalexy))

;; this doesn't work because
;; some steps mutate lines
;; need to actually have storage locations that are mutated
;; this "language" is turing complete
;; (def steps
;;   [(Line. [10 10] [20 20]) -> ?ls1
;;    (Line. (-> ?ls1
;;                   (snap-points)
;;                   :start)
;;               [400 400])])


(comment
  ;; alternatively
  ;; more keyboard macroish
  (def steps
    [(Line. [10 10] [20 20])
     (cycle-snap) (cycle-snap)
     (Line. (current-snap) [40 40])])
  )



(defr components
  [
   (let [lines (:lines (hydrate-steps steps))]
     (when (seq lines)
       (apply pen/group lines)))
   (rectangle 200 200)
   (vertical-layout
    (label (str "mode: " mode))
    (horizontal-layout
     (label (str "p1 "))
     (text-input p1-str
                 ~(fn [k]
                    (cond
                     (string? k)
                     (rv! p1-str (str @p1-str k))

                     (= :back k)
                     (rv! p1-str (subs @p1-str 0 (max 0 (dec (.length @p1-str)))))

                     :else nil)
                    
                    )))
    (label (str "m1 " (pr-str m1)))
    (button "reset" ~(fn []
                       (rv! steps [])))    
    (label (str "steps "))
    (when (seq steps)
      (let [steps-str (with-out-str
                        (clojure.pprint/pprint steps))
            step-lines (clojure.string/split steps-str #"\n")]
        (apply vertical-layout
               (for [sline step-lines]
                 (label sline :font-size 10))))))
   (key-thing
    ~(fn [key]
       (case key
         "l" (r! mode 'Line.)
         "r" (r! mode 'Rectangle.)
         "m" (r! mode :move)
         nil)))
   (mouse-thing
    ~(fn [mtype mx my]
       (when (or (> mx 200)
                 (> my 200))
        (when (= mtype :down)
          (when (= @mode :move)
            (if (nil? @m1)
              (let [[line-id snap-type]
                    (first
                     (for [[line-id line] (map-indexed vector (:lines (hydrate-steps @steps)))
                           snap-type (snap-points line)
                           :let [sp (snap-point line snap-type)]
                           :when (< (dist [mx my] sp) 50)]
                       [line-id snap-type]))]
                (when line-id
                  (rv! m1 [line-id snap-type])))
              (let [[line-id snap-type] @m1
                    closest-snap (first
                                  (for [[line-id line] (map-indexed vector (:lines (hydrate-steps @steps)))
                                        snap-type (snap-points line)
                                        :let [sp (snap-point line snap-type)]
                                        :when (< (dist [mx my] sp) 50)]
                                    `(snap-point (-> ~'canvas :lines (nth ~line-id))
                                                 ~snap-type)))
                    pt (if closest-snap
                         closest-snap
                         [mx my])]
                (msg pt)
                (dosync
                 (rv! m1 nil)
                 (ru! steps conj `(~'move-line ~line-id
                                               (v- ~pt
                                                   (snap-point (-> ~'canvas :lines (nth ~line-id))
                                                               ~snap-type)
                                                   )))))))

          (when (= @mode 'Line.)
            (let [closest-snap (first
                                (for [[line-id line] (map-indexed vector (:lines (hydrate-steps @steps)))
                                      snap-type (snap-points line)
                                      :let [sp (snap-point line snap-type)]
                                      :when (< (dist [mx my] sp) 50)]
                                  `(snap-point (-> ~'canvas :lines (nth ~line-id))
                                               ~snap-type)))
                  pt (if closest-snap
                       closest-snap
                       [mx my])
                  [err p1-val] (try
                                 [nil (read-string @p1-str)]
                                 (catch Exception e
                                   [e nil]))]
              (if err
                (msg "error parsing p1 ")
                (if (nil? p1-val)
                  (dosync
                   (rv! p1-str (pr-str pt)))
                  (dosync
                   (rv! p1-str "nil")
                   (ru! steps conj `(~'add-line (~(deref mode)  ~p1-val ~pt))))))))))))])




(defr event-handlers
  (pen/make-event-handlers components))













