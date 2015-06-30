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
            [minebot-clj.environment :as env :refer [defr r! rv! ru! r? with-renv]])
  (:require [minebot-clj.evaluable :refer
             [evaluate
              evaluable?
              ->ClojureEvaluable
              constant-evaluable
              dependencies]])
  (:require [clj-pdf.core :as pdf])
  (:require [environ.core :refer [env]])
  (:require [taoensso.faraday :as far])
  (:require [clojure.algo.graph :as graph])

  )

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

(defn key-handler2 [renv text val]
  (fn [s]
    (println text val)
    (cond
     (= s :return)
     (env/set-value renv val (env/get-renv-value renv text) #{})

     :else
     ((key-handler renv text) s))
    (dosync
     (env/shake! (deref (env/get-or-create-renv)) 'components))))

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
     (let [{:keys [x y name form val]} cell]
       
       (move x y
             (vertical-layout
              (text-input (str name))
              (text-input (str "form: " form)
                          (fn [k]
                            (println k)))
              (text-input (str "val: " val)
                          (fn [k]
                            (println k))))))))
  (def memo-cell-editor-components (memoize cell-editor-components))
 ;; (penumbra.app/repaint! @pen/current-app)
  )

(defcomponent CellEditor [name form val x y]
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

(defn cell-editor [name form val x y]
  (CellEditor. (make-cid "cell-editor" )
               name form val x y))

(defr cell-add? false)

(defr cells [])

(defr components
  [(mapv deref cells)
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
    ~(fn [mx my]
       (when (r? cell-add?)
         (let [myenv myenv]
           (dosync
            (with-renv myenv
              [form# '(+ 1 2 3)
               cell-name# "abc"
               val-str# (str "dunno")
               elem# (move
                      mx my
                      (horizontal-layout
                       (text-input cell-name# (key-handler myenv 'cell-name#))
                       (text-input form# (key-handler myenv 'form#))
                       (label val-str#)))]
              (env/set-form-and-deps
               myenv 'val-str# 
               (->ClojureEvaluable *ns* '(when-let [cname (env/get-renv-value minebot-clj.modicum/myenv 'cell-name#)]
                                           (str (env/get-renv-value minebot-clj.modicum/myenv
                                                                    (symbol cname)))) {})
               #{'cell-name# 'form#})
              (rv! cell-add? false)
              (ru! cells conj (env/get-renv-ref myenv 'elem#)))))

        #_(dosync
           (env/set-form-and-deps
            ~myenv cell-sym
            (->ClojureEvaluable *ns* form {})
            nil)
           (env/set-form-and-deps
            ~myenv editor-sym
            (->ClojureEvaluable *ns*
                                (list
                                 'cell-editor
                                 (str cell-sym)
                                 (list 'quote form)
                                 cell-sym
                                 mx my)
                                {})
            nil)
           
           (ru! cells
                conj
                (env/get-renv-ref ~myenv editor-sym))
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

;; (r! components
;;     (text-input ))

;; (r! components
;;     ;; nil initial value
;;     (text-input ^subname))

;; (r! components
;;     (text-input ^subname initial-val))

;; (r! components
;;     ;; anonymous with nil initial value
;;     (text-input ^))

;; (r! components
;;     ;; anonymous
;;     (text-input ^ initial-val))

;; (r! components
;;     ;; constant
;;     (text-input "adf"))



 
;; (defr cell-names ['a 'b 'c 'd 'e 'f] )

;; (r! myenv a 3)
;; (r! myenv a/sform "3")
;; (r! myenv b 6)
;; (r! myenv b/sform "0")
;; (r! myenv c 0)
;; (r! myenv c/sform "0")
;; (r! myenv d 0)
;; (r! myenv d/sform "0")
;; (r! myenv e (+ a b 2))
;; (r! myenv e/sform "(+ a b 2)")
;; (r! myenv f 0)
;; (r! myenv f/sform "0")
;; (r! error-str nil)
;; (rv! args #{})
;; (rv! args-sform (pr-str args))
;; (rv! ret nil)
;; (rv! ret-sform (pr-str ret))



;; (r! components
;;     (vertical-layout
;;      (apply
;;       vertical-layout
;;       (for [nm cell-names]
;;         (horizontal-layout
;;          (label (str nm))
;;          (spacer 20 0)
;;          (text-input (env/get-renv-ref ~myenv (symbol (name nm) "sform"))
;;                      (fn [s]
;;                        (cond
;;                         (= :return s)
;;                         (try
;;                           (dosync
;;                            (env/set-form-and-deps
;;                             ~myenv nm
;;                             (->ClojureEvaluable *ns*
;;                                                 (read-string
;;                                                  (env/get-renv-value ~myenv (symbol (name nm) "sform")))
;;                                                 {})
;;                             nil))
;;                           (r! error-str "")
;;                           (catch Exception e
;;                             (r! error-str ~(str e))))

;;                         :else
;;                         ((key-handler ~myenv (symbol (name nm) "sform")) s))))
;;          (spacer 20 0)
;;          (label (str (env/get-renv-value ~myenv nm)))
;;          #_(text-input ))))
;;      (label (str error-str))
;;      (horizontal-layout
;;       (text-input args-sform
;;                   (fn [s]
;;                     (cond
;;                        (= :return s)
;;                        (try
;;                          (rv! args (read-string args-sform))
;;                          (r! error-str "")
;;                          (catch Exception e
;;                            (r! error-str ~(str e))))

;;                        :else
;;                        ((key-handler 'args-sform) s))))
;;       (spacer 20 0)
;;       (label (pr-str args)))
;;      (horizontal-layout
;;       (text-input ret-sform
;;                   (fn [s]
;;                     (cond
;;                        (= :return s)
;;                        (try
;;                          (rv! ret (read-string ret-sform))
;;                          (r! error-str "")
;;                          (catch Exception e
;;                            (r! error-str ~(str e))))

;;                        :else
;;                        ((key-handler 'ret-sform) s))))
;;       (spacer 20 0)
;;       (label (pr-str ret)))
;;      (when made-fn-str
;;        (apply
;;         vertical-layout
;;         (for [line (clojure.string/split-lines made-fn-str)]
;;           (label line))))
     
;; ))


(defn make-fn [renv args ret]
  (let [sorted-deps (apply concat
                           (graph/dependency-list
                            (struct graph/directed-graph
                                    (keys (:deps renv))
                                    (fn [node]
                                      (-> renv :deps (get node))))))
        bindings (for [dep sorted-deps
                       :when (not (or (contains? args dep)
                                      (namespace dep)))]
                   [dep (-> renv :evaluables (get dep) :form)])
        
        ]
    `(fn [~@args]
       (let [~@(apply concat bindings)]
         ~ret))))


(r! made-fn-str
    (with-out-str
      (clojure.pprint/pprint
       (make-fn myenv args ret))))
