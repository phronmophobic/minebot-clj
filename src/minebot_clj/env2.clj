(ns minebot-clj.env2
  (:require [clojure.algo.graph :as graph]
            [minebot-clj.analyze :refer [cell-deps]]
            [minebot-clj.penumbra :as pen
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
                     start]]
            [seesaw.mig :as mig]
            [seesaw.bind :as sbind]
            [seesaw.core :as seesaw]))
 ;; (use 'seesaw.dev)

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


(defn assoc-pod [pg sym f deps]
  (assoc pg sym [f deps]))

(defn dissoc-pod [pg sym]
  (dissoc pg sym))

(defn evaluate [evaluable deps]
  (if (nil? deps)
    evaluable
    (apply evaluable deps)))

(defn depgraph [pg]
  (graph/dependency-list
   (struct graph/directed-graph
           (keys pg)
           (fn [node]
             (-> pg
                 (get node)
                 second))))
  )

(defn evalpg
  ([pg]
   (evalpg pg nil))
  ([pg wanted]
   (let [sorted-deps (apply concat
                            (graph/dependency-list
                             (struct graph/directed-graph
                                     (keys pg)
                                     (fn [node]
                                       (-> pg
                                           (get node)
                                           second)))))
         sorted-deps (cond

                      (nil? wanted)
                      sorted-deps

                      :else
                      (loop [[dep & more] sorted-deps
                             sorted-deps []
                             wanted (if (symbol? wanted) (hash-set wanted) (set wanted))]
                         (if (and dep (seq wanted))
                           (recur more (conj sorted-deps dep) (disj wanted dep))
                           sorted-deps)))]
     (loop [result {}
            [sym & more :as all] sorted-deps]
       (if sym
         (let [deps (-> pg (get sym) second)
               dep-vals (when deps
                          (for [dep deps]
                            (get result dep)))
               evaluable (-> pg (get sym) first)]
           (recur (assoc result
                    sym
                    (evaluate evaluable dep-vals))
                  more))
         result)))))



(def mypg {'b [(fn [a]
                 (+ a 2))
               ['a]]
           'a [(fn []
                 10)
               []]
           'c [(fn [a b]
                 (* a b))
               ['a 'b]]}
  )

(defn unquoted [form]
  (let [branch? (fn [form]
                  (and (minebot-clj.analyze/seqable? form)
                       ;; probably need some tests for nests (nesting)
                       ;; also probably need to put more thought into this.
                       (not (#{'r! 'with-renv 'rv! 'ru! 'clojure.core/unquote} (first form)))))
        subforms (tree-seq branch? seq form)]
    (for [subform subforms
          :when (and (seq? subform)
                     (= (first subform) 'clojure.core/unquote))]
      subform)))

(def ^:dynamic *pg* (atom {}))

(defn -r! [pg name form env-keys]
  (let [qforms (set (unquoted form))
        smap (into {}
                   (for [qform qforms]
                     [qform (gensym)]))
        requoted-form (clojure.walk/postwalk-replace
                       smap
                       form)
        locals (set (concat
                     env-keys
                     (keys smap)))
        deps (cell-deps form)
        deps (remove #(contains? locals
                                 %)
                     deps)
        deps (remove (fn [name]
                       (when-let [var (ns-resolve *ns* name)]
                         (-> var meta :reactive? not)))
                     deps)
        deps (remove #(= % name) deps)]
    (let [f (eval `(fn [~@deps]
                     ~requoted-form))]
      (assoc pg name [f deps]))))

(defmacro r! [name form]
  `(swap! *pg* -r! (quote ~name) (quote ~form) (keys ~&env)))

#_(defmacro r! [name form]
 (let [
        qforms (set (unquoted form))
        smap (into {}
                   (for [qform qforms]
                     [qform (gensym)]))
        requoted-form (clojure.walk/postwalk-replace
                       smap
                       form)
        locals (set (concat
                     (keys &env)
                     (keys smap)))
        deps (cell-deps form)
        deps (remove #(contains? locals
                                 %)
                     deps)
        deps (remove (fn [name]
                       (when-let [var (ns-resolve *ns* name)]
                         (-> var meta :reactive? not)))
                     deps)
        deps (remove #(= % name) deps)]
    `(let [f# (fn [~@deps]
               ~requoted-form)]
       (swap! *pg* assoc (quote ~name)
              [f#
               [~@(for [dep deps]
                    (list 'quote dep))]]))))


(defmacro rv! [name form]
  `(swap! *pg* assoc (quote ~name) [~form nil]))

(defmacro ru! [name f & args]
  `(swap! *pg* update-in [(quote ~name) 0] ~f ~@args))


(def components (ref []))
(def event-handlers (ref []) )
(add-watch *pg* :repainting
           (fn [_ _ _ _]
             (let [pg (evalpg @*pg*)]
               (dosync
                (ref-set components (get pg 'comps))
                (ref-set event-handlers (get pg 'event-handlers))))))

(rv! mytext "hello")

(r! comps [(apply
            vertical-layout
            (for [_ (range 8)]
              (text-input mytext
                          (fn [key]
                            (time
                             (ru!
                              mytext
                              (fn [old-text]
                                (println (let [new-text
                                               (cond
                                                (= :back key)
                                                (subs old-text 0 (max 0 (dec (.length old-text))))
                                                        
                                                (string? key)
                                                (str old-text key))]
                                           (if new-text
                                             new-text
                                             old-text)))
                                (let [new-text
                                      (cond
                                       (= :back key)
                                       (subs old-text 0 (max 0 (dec (.length old-text))))
                                               
                                       (string? key)
                                       (str old-text key))]
                                  (if new-text
                                    new-text
                                    old-text)))))))))])
(r! event-handlers (pen/make-event-handlers comps)) 


(clojure.pprint/pprint
 (macroexpand '(r! b (+ a 1 c))))

;; (r! b (+ a 1 c ))

;; (r! a 20)
;; (let [foo 10]
;;   (r! c (* a foo)))

(def out (atom false))
(def widgets (atom []))

(defn ui-to-name-vals! []
  (seesaw/invoke-now
   (into {}
         (seesaw/invoke-now
          (try
            (for [{:keys [name formula]} @widgets]
              [(seesaw/value name) (seesaw/value formula)])
            (catch Exception e
              (msg e)))))))

(defn ui-to-pg! []
  (seesaw/invoke-now
   (let [name-vals (ui-to-name-vals!)
         pg (reduce
             (fn [pg [key val]]
               (if (not= "" val)
                 (-r! pg (symbol key) (read-string val) #{})
                 pg))
             {}
             name-vals)]
     pg)))


(defn ui-from-name-vals! [name-vals]
  (seesaw/invoke-now
   (try
     (loop [[widget & more-widgets] @widgets
            [elem & more-elems] name-vals]
       (if (and elem widget)
         (do
           (let [{:keys [key formula]} widget
                 [name val] elem]
             (seesaw/value! key name)
             (seesaw/value! formula val))
           (recur more-widgets more-elems))))
     (catch Exception e
       (msg e)))))

(defn eval-ui! []
  (seesaw/invoke-now
   (let [name-vals (into {}
                         (seesaw/invoke-now
                          (try
                           (for [{:keys [name formula]} @widgets]
                             [(seesaw/value name) (seesaw/value formula)])
                           (catch Exception e
                             (msg e)))))
         pg (reduce
             (fn [pg [key val]]
               (if (not= "" val)
                 (-r! pg (symbol key) (read-string val) #{})
                 pg))
             {}
             name-vals)]
     (evalpg pg))))



(defn truncated [s n]
  (let [len (min 15 (.length s))]
    (subs s 0 len)))

(defn seemake []
  (let [new-widgets
        (for [i (range 20)]
          {:name (seesaw/text :text "" :columns 20) 
           :formula (seesaw/text :text "" :columns 10) 
           :val (seesaw/label :text "adf")})]

    (reset! widgets new-widgets)
    (seesaw/frame :title "Hello",
                  :content 
                  (mig/mig-panel
                   :constraints
                   ["" "" "0[]0"]
                   :items
                   (concat
                    (apply
                     concat
                     (for [{:keys [name formula val]} new-widgets]
                       [
                        [name]
                        [formula]
                        [val "width 100::, wrap"]]))
                    [
                     [(seesaw/button :text "eval"
                                     :listen
                                     [:action
                                      (fn [e]
                                        (seesaw/invoke-now
                                         (try
                                           (let [results (eval-ui!)]
                                             (doseq [{:keys [name val]} @widgets]
                                               (let [calculated-value (get results (symbol (seesaw/value name)))]
                                                 (seesaw/value! val (truncated (pr-str calculated-value)
                                                                               30)))))
                                           (catch Exception e
                                             (msg e)))))]
                                     ) "span,align right"]])))
    ))



(def root (atom nil))

(seesaw/invoke-later
 (msg (seesaw/select @root [:JLabel])))

(defn see [& args]
  (let [out *out*]
   (seesaw/invoke-later
    (try
      (reset! root (seemake))
      (-> @root
          seesaw/pack!
          seesaw/show!)
      (catch Exception e
        (binding [*out* out]
          (clojure.stacktrace/print-stack-trace e)))))))















