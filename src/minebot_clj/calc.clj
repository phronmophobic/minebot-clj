(ns minebot-clj.calc)
(use 'minebot-clj.cell)

(def env2 (atom (minebot_clj.cell.Environment. {} {} {})))

(swap! env2 set-val 'text "")
(swap! env2 set-val 'env-vals (fn []
                                (:vals @env2)))
(swap! env2 set-val 'set-val
       (fn
         ([k v]
            (swap! env2 set-val k v))
         ([kvs]
            (swap! env2
                   (fn [env]
                     (reduce (fn [env [k v]]
                               (set-val env k v))
                             env
                             kvs))))))
(swap! env2 set-val 'truncates (fn [s n]
                                 (subs s 0 (min n (count s)))))
(swap! env2 set-val 'find-child (fn [ref name]
                                  (.findChild (.window ref) nil name)))
(swap! env2 set-val 'msg msg)
(swap! env2 set-val 'set-form #(swap! env2 set-form %1 %2))
(swap! env2 set-val 'shake (fn [name]
                             (swap! env2 shake name)))

(defmacro defenv2 [sym body]
  `(swap! env2 set-form (quote ~sym) (quote ~body)))

(defenv2 current-num 0)
(defenv2 current-op nil)
(defenv2 next-num nil)
(swap! env2
       (fn [env2]
         (-> env2
             (set-val 'show-ui show-ui)
             (set-form 'ui '[:QVBoxWidget
                             [:QLabel {:text (if next-num
                                               (str next-num)
                                               (str current-num))}]
                             [:QLabel {:text (str "next " (pr-str next-num))}]
                             [:QLabel {:text (str "current " (pr-str current-num))}]
                             [:QLabel {:text (str "op " (pr-str current-op))}]
                             [:QHBoxWidget
                              (for [i (range 10)]
                                [:QPushButton {:text (str i)
                                               :clicked (fn []
                                                          (if current-op
                                                            (set-val 'next-num (+ i (* 10 next-num)))
                                                            (set-val 'current-num (+ i (* 10 current-num)))))}])]
                             [:QHBoxWidget
                              (for [op [#'+ #'-]]
                                [:QPushButton {:text (-> op meta :name name)
                                               :clicked
                                               (fn []
                                                 (set-val {'next-num 0
                                                           'current-op op})
                                                 )}])]
                             [:QHBoxWidget
                              [:QPushButton {:text "="
                                             :clicked
                                             (fn []
                                               (when (and next-num
                                                          current-op)
                                                 (set-val 'current-num
                                                          (current-op current-num next-num))
                                                 (set-val 'next-num nil)
                                                 (set-val 'current-op nil)))}]]
                             [:QPushButton {:text "Clear"
                                            :clicked
                                            (fn []
                                              (set-val {'current-num 0
                                                        'next-num nil
                                                        'current-op nil}))}
                              ]])
             (set-form 'make-ui! '(show-ui :145 ui)))))
