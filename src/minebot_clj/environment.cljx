(ns minebot-clj.environment
  (:require
   clojure.tools.reader
   [minebot-clj.evaluable :refer
    [evaluate
     evaluable?
     ->ClojureEvaluable
     constant-evaluable
     dependencies]]
   minebot-clj.analyze)

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

(defn reset-env!
  ([]
   (dosync
    (reset-env! (get-or-create-renv))))
  ([renv]
   (dosync
    (ref-set renv
             (Environment. {} {} {})))))

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
     (-> renv deref :refs (get sym)))))

(defn set-value
  ([name form locals]
   (set-value (get-or-create-renv)
              name
              form
              locals))
  ([renv name form locals]
   (dosync
    (let [evaluable (coerce-evaluable form locals)
          val (eval-in @renv evaluable (reactive-deps evaluable name))
          evaluable (constant-evaluable val)]
      (set-form-and-deps renv name evaluable {})))))

(defn get-renv-ref
  ([name]
   (get-renv-ref (get-or-create-renv) name))
  ([renv name]
   (-> @renv
       :refs
       (get name))))

(defn get-renv-deps
  ([name]
   (get-renv-deps (get-or-create-renv) name))
  ([renv name]
   (-> @renv
       :deps
       (get name))))

(defn get-renv-form
  ([name]
   (get-renv-form (get-or-create-renv) name))
  ([renv name]
   (-> @renv
       :evaluables
       (get name)
       :form)))

(defn get-renv-value
  ([name]
   (get-renv-value (get-or-create-renv) name))
  ([renv name]
   (when-let [val-ref (get-renv-ref renv name)]
     (deref val-ref))))

(defn update-value
  ([name f & args]
   (let [[renv name f args] (if (symbol? name)
                              [(get-or-create-renv) name f args]
                              (do
                                (assert (>= (count args) 1) "Bad args]")
                                [name f (first args) (rest args)]))]
    (dosync
     (let [old-val (get-renv-value renv name)
           new-val (apply f old-val args)
           evaluable (constant-evaluable new-val)]
       (set-value renv name evaluable nil))))))

(defn unquoted [form]
  (let [branch? (fn [form]
                  (and (minebot-clj.analyze/seqable? form)
                       ;; probably need some tests for nests (nesting)
                       ;; also probably need to put more thought into this.
                       (not (#{'r! 'with-renv 'rv! 'ru!} (first form)))))
        subforms (tree-seq branch? seq form)]
    (for [subform subforms
          :when (and (seq? subform)
                     (= (first subform) 'clojure.core/unquote))]
      subform)))

(defmacro r!
  ([renv name form]
   (let [;; not sure why i was macro expanding before :-/. is this necessary?
         ;; form (clojure.walk/macroexpand-all form)
         qforms (set (unquoted form))
         smap (into {}
                    (for [qform qforms]
                      [qform (gensym)]))
         requoted-form (clojure.walk/postwalk-replace
                        smap
                        form)]

     `(set-form-and-deps ~renv
                         (quote ~name)
                         (coerce-evaluable (quote ~requoted-form)
                                          (into {}
                                                (concat
                                                 [~@(for [[k _] &env]
                                                      [(list 'quote k)
                                                       k])]
                                                 [~@(for [[qform sym] smap]
                                                      [(list 'quote sym) (second qform)])])))
                         nil)))
  ([name form]
   `(dosync (r! (get-or-create-renv) ~name ~form))))

(defmacro r?
  ([renv name]
   `(get-renv-value ~renv (quote ~name)))
  ([name]
   `(get-renv-value (quote ~name))))



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



(defmacro with-renv
  ([renv bindings & body]
   (assert (vector? bindings) "a vector for its binding")
   (assert (even? (count bindings)) "an even number of forms in binding vector")
   (let [bind-pairs (partition 2 bindings)
         gensym-map-sym (gensym "gensym-map__")]
     ;; some code duplication with r!
     `(let [renv# ~renv
            locals# [~@(for [[k _] &env]
                         [(list 'quote k)
                          k])]
            ~gensym-map-sym (hash-map
                             ~@(apply
                                concat
                                (for [[sym _] bind-pairs
                                      :when (-> sym name (.endsWith "#"))
                                      :let [sname (name sym)
                                            prefix (str (subs sname 0 (dec (count sname))) "__")]]
                                  [(list 'quote sym)
                                   `(gensym ~prefix)])))
            defs# [~@(for [[sym form] bind-pairs
                           :let [;; not sure why i was macro expanding before :-/. is this necessary?
                                 ;; form (clojure.walk/macroexpand-all form)
                                 qforms (set (unquoted form))
                                 smap (into {}
                                            (for [qform qforms]
                                              [qform (gensym)]))
                                 form (clojure.walk/postwalk-replace
                                       smap
                                       form)]]
                       [`(get ~gensym-map-sym (quote ~sym) (quote ~sym))
                        `(clojure.walk/postwalk-replace
                          ~gensym-map-sym
                          (quote ~form))
                        (vec
                         (for [[qform sym] smap]
                           [(list 'quote (list 'quote sym))
                            `(clojure.walk/postwalk-replace
                              ~gensym-map-sym
                              (quote ~(second qform)))]))])]]
        (doseq [[sym# form# unquoteds#] defs#]
          (set-form-and-deps renv#
                             sym#
                             (->ClojureEvaluable *ns* form#
                                                 (into
                                                  {}
                                                  (concat locals# (eval unquoteds#))))
                             nil))))))

