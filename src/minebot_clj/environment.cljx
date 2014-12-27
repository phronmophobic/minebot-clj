(ns minebot-clj.environment
  (:require [minebot-clj.evaluable :refer [evaluate]])
  )


#+clj
(let [out *out*]
  (defn msg [& s]
    (binding [*out* out]
      (apply println s))))
#+cljs
(defn msg [& s]
  (apply js/console.log s))

(defprotocol IEnvironment
  (shake! [this name])
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

  (shake! [this name]
    (let [deps (get deps name)]
      ;; (msg "deps:" deps)
      (if (not (every? #(contains? refs %) deps))
        (do
          (msg "couldn't find args "
               (for [dep deps
                     :when (not (contains? refs dep))]
                 dep))
          nil)
        (let [evaluable (get evaluables name)
              bindings (into {}
                             (for [dep deps
                                   :let [val (-> refs (get dep) deref)]]
                               [dep val]))
              ref (get refs name)
              old-val (deref ref)
              new-val (evaluate evaluable bindings)]
          ;; (msg "updating " name)
          (when (not= old-val new-val)
            (set-ref! ref new-val)
            (doseq [[other-name other-deps] (:deps this)
                    :when (contains? other-deps name)]
              (shake! this other-name))))))))

(defn environment []
  (Environment. {} {} {}))
