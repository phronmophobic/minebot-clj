(ns cell.core)


(defprotocol IRefEnvironment
  (shake! [this name])
  (set-ref [this name ref])
  (set-form [this name evaluable dep]))


(defrecord RefEnvironment [evaluables deps refs]
  IRefEnvironment
  (set-ref [this name ref]
    (RefEnvironment. evaluables
                     deps
                     (assoc refs name ref)))
  (set-form [this name evaluable dep]
    (let [evaluables (assoc evaluables name evaluable)
          deps (assoc deps name (set dep))]
      (RefEnvironment. evaluables
                       deps
                       refs)))

  (shake! [this name]
    (let [deps (get deps name)
          ]
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
            (ref-set ref new-val)
            (doseq [[other-name other-deps] (:deps this)
                    :when (contains? other-deps name)]
              (shake! this other-name)))))))
  
  )
