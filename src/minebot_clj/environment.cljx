(ns minebot-clj.environment
  (:require [minebot-clj.evaluable :refer
             [evaluate
              evaluable?
              ->ClojureEvaluable
              constant-evaluable
              dependencies]])

  )


#+clj
(defn msg [& s]
  (apply println s))

#+cljs
(defn msg [& s]
  (apply js/console.log s))

(defprotocol IEnvironment
  (shake! [this name])
  (ready-for-eval? [this evaluable deps])
  (eval-in [this evaluable deps])
  (set-ref [this name ref])
  (set-form [this name evaluable dep]))

(defprotocol EnvironmentRef
  (set-ref! [this val]))

(extend-protocol EnvironmentRef
  #+clj
  clojure.lang.Ref
  #+clj
  (set-ref! [this val]
    (ref-set this val))

  #+clj clojure.lang.Atom
  #+cljs cljs.core/Atom
  (set-ref! [this val]
    (reset! this val)))

(defn reactive-deps [evaluable sym]
  (-> (dependencies evaluable)
      (->> (remove (fn [name]
                     (when-let [var (ns-resolve *ns* name)]
                       (-> var meta :reactive? not)))))
      set 
      (disj sym)))

(defrecord Environment [evaluables deps refs]
  IEnvironment
  (set-ref [this name ref]
    (Environment. evaluables
                     deps
                     (assoc refs name ref)))
  (set-form [this name evaluable dep]
    (let [evaluables (assoc evaluables name evaluable)
          deps (assoc deps name (set dep))]
      (Environment. evaluables
                       deps
                       refs)))
  (ready-for-eval? [this evaluable deps]
    (every? #(contains? refs %) deps))

  (eval-in [this evaluable deps]
    (let [bindings (into {}
                         (for [dep deps
                               :let [val (-> refs (get dep) deref)]]
                           [dep val]))]
      (evaluate evaluable bindings)))

  (shake! [this name]
    (let [evaluable (get evaluables name)
          deps (reactive-deps evaluable name)]
      (if (not (ready-for-eval? this evaluable deps))
        (do
          (msg "couldn't find args "
               (for [dep deps
                     :when (not (contains? refs dep))]
                 dep))
          nil)
        (let [ref (get refs name)
              old-val (deref ref)
              new-val (eval-in this evaluable deps)]
          (when (not= old-val new-val)
            (set-ref! ref new-val)
            (doseq [[other-name other-deps] (:deps this)
                    :when (contains? other-deps name)]
              (shake! this other-name))))))))

(defn environment []
  (Environment. {} {} {}))

;; map ns to env
(def all-envs (ref {}))


(defn get-or-create-renv []
  (when-not (contains? @all-envs *ns*)
      (ref-set all-envs
               (assoc @all-envs *ns* (ref (environment)))))
  (get @all-envs *ns*))

(defn coerce-evaluable [form locals]
  (if (evaluable? form)
    form
    (->ClojureEvaluable *ns* form
                        locals)))

(defn set-form-and-deps
  ([sym evaluable]
   (set-form-and-deps sym evaluable nil))
  ([sym evaluable deps]
   (dosync
    (set-form-and-deps (get-or-create-renv) sym evaluable deps)))
  ([renv sym evaluable deps]
   (let [deps (if (nil? deps)
                (reactive-deps evaluable sym)
                deps)]
     (dosync
      ;; maybe define ref separately and not auto create
      (when-not (contains? (:refs @renv) sym)
        (ref-set renv (set-ref @renv sym (ref nil))))
      (ref-set renv (set-form @renv sym evaluable deps))
      (shake! (deref renv) sym))
     (-> @renv :refs (get sym)))))

(defn set-value [name form locals]
  (dosync
   (let [renv (deref (get-or-create-renv))
         evaluable (coerce-evaluable form locals)
         val (eval-in renv evaluable (reactive-deps evaluable name))
         evaluable (constant-evaluable val)]
     (set-form-and-deps name evaluable {}))))

(defn get-renv-ref [name]
  (let [renv (get @all-envs *ns*)]
    (-> @renv
        :refs
        (get name))))

(defn get-renv-value [name]
  (deref (get-renv-ref name)))

(defn update-value [name f & args]
  (dosync
   (let [old-val (get-renv-value name)
         new-val (apply f old-val args)
         evaluable (constant-evaluable new-val)]
     (set-value name evaluable nil))))

(defmacro r! [name form]
  `(set-form-and-deps (quote ~name)
                      (coerce-evaluable (quote ~form)
                                        (into {}
                                              [~@(for [[k _] &env]
                                                   [(list 'quote k)
                                                    k])]))))

(defmacro r? [name]
  `(get-renv-value (quote ~name)))



(defmacro rv! [name form]
  `(set-value (quote ~name) (quote ~form) (into {}
                                                [~@(for [[k _] &env]
                                                     [(list 'quote k)
                                                      k])])))

(defmacro ru! [name fn & args]
  `(update-value (quote ~name) ~fn ~@args))


(defmacro defr [name form]
  `(do
     (r! ~name ~form)
     (def ~(vary-meta name assoc :reactive? true) (get-renv-ref (quote ~name)))))

