(ns minebot-clj.modicum
  (:require [minebot-clj.penumbra :as pen
             :refer [IComponent
                     defcomponent
                     IBounds
                     IMouseDown
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
            [minebot-clj.environment :as env :refer [defr r! rv! ru! r?]])
  (:require [minebot-clj.evaluable :refern
             [evaluate
              evaluable?
              ->ClojureEvaluable
              constant-evaluable
              dependencies]])
  (:require [clj-pdf.core :as pdf])
  (:require [environ.core :refer [env]])
  (:require [taoensso.faraday :as far]))

(defn mouse-thing [mouse-fn]
  (reify
    IComponent
    (cid [this]
      (pen/make-cid "mouse-thing"))
    IDraw
    (draw [this state]
      nil)
    IMouseDown
    (-mouse-down [this [mx my]]
      (mouse-fn mx my))))

(defn key-thing [key-fn]
  (reify
    IComponent
    (cid [this]
      (pen/make-cid "mouse-thing"))
    IDraw
    (draw [this state]
      nil)
    IKeyPress
    (-key-press [this key]
      (key-fn key))))

(defr myenv (env/environment))


;; (env/set-value myenv 'a '2 {})
;; (env/set-form-and-deps
;;  myenv 'b
;;  (->ClojureEvaluable *ns* '(+ 10 a) {})
;;  #{'a})
;; (env/set-form-and-deps
;;  myenv 'c
;;  (->ClojureEvaluable *ns* '(* 1.5 (+ a b)) {})
;;  #{'a 'b})

(do
  (defn cell-editor-components
    ([cell]
     (cell-editor-components cell nil))
    ([cell state]
     (let [{:keys [x y name text]} cell]
       (move x y
             (vertical-layout
              (text-input (str name))
              (text-input (str "text: " text)
                          (fn [k]
                            (println k))))))))
  (def memo-cell-editor-components (memoize cell-editor-components))
 ;; (penumbra.app/repaint! @pen/current-app)
  )

(defcomponent CellEditor [name text x y]
  IBounds
  (-bounds [this]
    (let [comps (memo-cell-editor-components (into {} this))
          [ox oy] (origin comps)
          [w h] (bounds comps)]
      [(+ ox w)
       (+ oy h)]))
  ;; IBounds
  ;; (-bounds [this]
  ;;   (bounds (cell-editor-components e this)))
  ;; IOrigin
  ;; (-origin [this]
  ;;   (origin (cell-editor-components this)))
  IChildren
  (-children [this]
    [(memo-cell-editor-components (into {} this))])
  IDraw
  (draw [this state]
    (draw (memo-cell-editor-components (into {} this))
          state)))

(defn cell-editor [name text x y]
  (CellEditor. (make-cid "cell-editor" )
               name text x y))

(defr cell-add? false)

(defr cells [])

(defr components
  [cells
   ;;   (text-input "yo")
   (mouse-thing
    (fn [mx my]
      (println mx my)))
   (key-thing
    (fn [k]
      (when (and (= k "a")
                 (nil? pen/*focus*))
        (ru! cell-add? not))))
   (mouse-thing
    (fn [mx my]
      (when cell-add?
        (dosync
         (ru! cells
              conj
              (cell-editor  (~ "") mx my ))
         (rv! cell-add? false)))))
   (vertical-layout
    (label (str cell-add?))
    (label (str (into {}
                      (for [[k r] (:refs myenv)]
                        [k (deref r)])) ))


    )])
(add-watch myenv :myrepaint
           (fn [k r old new]
             (dosync
              (env/shake! (deref (env/get-or-create-renv)) 'components))))
(defr event-handlers
  (pen/make-event-handlers components))

(r! components
    (text-input ))

(r! components
    ;; nil initial value
    (text-input ^subname))

(r! components
    (text-input ^subname initial-val))

(r! components
    ;; anonymous with nil initial value
    (text-input ^))

(r! components
    ;; anonymous
    (text-input ^ initial-val))

(r! components
    ;; constant
    (text-input "adf"))


(defmacro badmacro [form]
  (println form))
