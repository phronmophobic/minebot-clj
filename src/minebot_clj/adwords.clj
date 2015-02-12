(ns minebot-clj.adwords
  (:require [minebot-clj.cell :as cell :refer [show-ui]])
  (:use [minebot-clj.evaluable])
  (:require [minebot-clj.environment :as env :refer [set-ref shake! set-form]]))


(def renv (atom (env/environment)))
(defmacro r! [name form]
  `(do
     (let [form# (quote ~form)
           evaluable# (if (evaluable? form#)
                        form#
                        (->ClojureEvaluable *ns* form#
                                            (into {}
                                                  [~@(for [[k _] &env]
                                                       [(list 'quote k)
                                                        k])])))
           deps# (-> (dependencies evaluable#)
                     (->> (remove (fn [name#]
                                    (when-let [var# (ns-resolve *ns* name#)]
                                      (-> var# meta :reactive? not)))))
                     set 
                     (disj ~name))]
       (swap! renv set-form (quote ~name) evaluable# deps#)
       (dosync
        (shake! (deref renv) (quote ~name)))
       (when (instance? clojure.lang.IDeref ~name)
         (deref ~name)))))
(defmacro defr [name form]
  `(do
     (defonce ~(vary-meta name assoc :reactive? true) (ref nil))
     (swap! renv set-ref (quote ~name) ~name)
     (r! ~name ~form)))


(defr a 1)
(defr b 2)


(defr keywords
  (->> (map #(clojure.string/split % #",")
            (clojure.string/split (slurp (clojure.java.io/resource "keywords.txt"))
                                  #"\n"))
       (sort-by second)))

(defr grouped
  (->> keywords
       (map first)
       #_(group-by identity)
       #_(filter #(> (count %) 1))))

(defr dups
  )

(defr ui2!
  (show-ui :ui2
           [:QScrollArea
            {:viewport
             [:QLabel {:text (with-out-str
                               (clojure.pprint/pprint grouped))}]}]))

(defr ui!
  (show-ui :keywords
           [:QScrollArea
            {:viewport 
             [:QVBoxWidget
              (for [[group enabled text] keywords]
                [:QHBoxWidget
                 [:QLabel {:text group}]
                 [:QLabel {:text enabled}]
                 [:QLabel {:text text}]
                 ])]}]))

