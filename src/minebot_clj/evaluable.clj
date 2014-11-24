(ns minebot-clj.evaluable
  (:require [clj-rhino :as js]
            [minebot-clj.analyze :refer [cell-deps]])

  (:import org.mozilla.javascript.CompilerEnvirons
           org.mozilla.javascript.Parser
           org.mozilla.javascript.ast.NodeVisitor
           ))




(defprotocol Evaluable
  (form [this])
  (-evaluate [this env]))

(defn evaluate
  ([evaluable]
     (-evaluate evaluable nil))
  ([evaluable env]
     (-evaluate evaluable env)))

(defprotocol Dependable
  (dependencies [this]))

(def ^:dynamic *locals*)
(defrecord ClojureEvaluable [ns form locals]
  Evaluable
  (form [this]
    (pr-str form))
  (-evaluate [this env]
    (let [env (merge env locals)
          bindings (for [k (keys env)]
                     [k `(get *locals* (quote ~k))])
          eval-form `(let [~@(apply concat bindings)]
                       ~form)]
      ;; (minebot-clj.cell/msg "evaling " ns eval-form)
      (binding [*ns* ns
                *locals* env]
        (eval
         eval-form))))

  Dependable
  (dependencies [this]
    (let [deps (cell-deps form)
          deps (remove #(contains? (set (keys locals))
                                   %)
                       deps)]

      deps)))

(defn clojure-evaluable [ns form locals])



(defn js-eval-str [jss]
  (str "(function(){" jss "})()"))
(defrecord JavascriptEvaluable [jss]
  Evaluable
  (form [this]
    jss)
  (-evaluate [this env]
    (let [scope (js/new-safe-scope)
          eval-str (js-eval-str jss)]
      (doseq [[var val] env]
        (js/set! scope (name var) val))
      (js/eval scope
               eval-str)))
  Dependable
  (dependencies [this]
    (let [env (CompilerEnvirons.)
          node (.parse (Parser. env) (js-eval-str jss) "source" 1)
          cell-deps (atom #{})]
      (.visitAll node
                 (reify
                   NodeVisitor
                   (visit [this node]
                     (when-let [parent (try (.getParent node)
                                            (catch Exception e))]
                       (when (and (instance? org.mozilla.javascript.ast.Name node )
                                  (not (instance? org.mozilla.javascript.ast.VariableInitializer parent)))
                         (swap! cell-deps conj (symbol (.getIdentifier node)))))
                     ;; 
                     ;; (println node (type node) (str node) (.getNthParent node 0))
                     true)))
      @cell-deps)))


(defn evaluable? [x]
  (satisfies? Evaluable x))











