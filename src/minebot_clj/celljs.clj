(ns minebot-clj.celljs
  (:require [cljs.closure :as cljsc]
            [minebot-clj.cell :as cell])
  (:use [minebot-clj.evaluable])
  (:require [cljs.compiler :as comp]
            [cljs.env :as env]
            [cljs.analyzer :as ana]))

;;(require 'cljs.compiler)

;; (require '[cljs.closure :as cljsc])

;; (cljsc/build "samples/hello/src" {:output-dir "samples/hello/out" :output-to "samples/hello/hello.js"})

;; (cljsc/-compile '(defn plus-one [x] (inc x)) {})

;; (cljsc/build '[(ns hello.core)
;;                (defn ^{:export greet} greet [n] (str "Hola " n))
;;                (defn ^:export sum [xs] 42)]
;;              {})


;; (cljsc/build '[(ns hello.core) (def a 1)] {})

;; (println (cljs.clojure/build '[(ns hello.core)
;;                     (defn ^{:export greet} greet [n] (str "Hola " n))
;;                     (defn ^:export sum [xs] 42)]
;;                              {:optimizations :simple :pretty-print true}))

;; (env/with-compiler-env (env/default-compiler-env)
;;   (println (cljsc/build
;;             (vec (ana/forms-seq "/Users/adrian/workspace/minebot-clj/src/minebot_clj/environment.cljs"))
;;             {:optimizations :simple :pretty-print true})))


;; (println (cljsc/build '[(ns hello.core)
;;                         (defn ^{:export greet} greet [n] (for [i (range 9)]
;;                                                            i))
                        
;;                         (atom nil)
;;                         (defn ^:export sum [xs] 42)]
;;                       {:optimizations :simple :pretty-print true}))

;; {:ns nil,
;;  :context :statement,
;;  :locals {},
;;  :js-globals
;;  {console {:name console},
;;   location {:name location},
;;   escape {:name escape},
;;   screen {:name screen},
;;   global {:name global},
;;   process {:name process},
;;   require {:name require},
;;   alert {:name alert},
;;   history {:name history},
;;   window {:name window},
;;   module {:name module},
;;   exports {:name exports},
;;   document {:name document},
;;   navigator {:name navigator},
;;   unescape {:name unescape}}}

;; (def myenv (env/default-compiler-env))


;; (println
;;  (env/with-compiler-env myenv
;;    (comp/with-core-cljs nil
;;      (fn []
;;        (comp/emit-str (ana/analyze
;;                        (ana/empty-env)
;;                        '(+ 1 2)))))))


;; (println
;;  (cljsc/build '[(ns hello.core)
;;                 (defrecord Jose [a b c])
;;                 (defn ^:export sum [xs] 42)]
;;               {:optimizations :simple :pretty-print true}

;;               myenv))

;; (defmacro aa [n]
;;   `(+ ~n ~n ~n ~n))
;; ;; testmacros
;; (println
;;  (cljsc/build '[(ns hello.core
;;                   (:require-macros [minebot-clj.celljs :as celljs]))
;;                 (defrecord Jose [a b c])
;;                 (defn ^:export sum [xs] 42)
;;                 (def ^:export myas (celljs/aa 2))]
;;               {:optimizations :simple :pretty-print true}

;;               myenv))

;; (println
;;  (env/with-compiler-env myenv
;;    (comp/with-core-cljs nil
;;      (fn []
;;        (comp/emit-str (ana/analyze
;;                        (merge (ana/empty-env)
;;                               {:ns (@ana/namespaces 'hello.core)})
;;                        '(summer [1 2])))))))






;; (println
;;  (env/with-compiler-env (env/default-compiler-env)
;;    (cljsc/compile-form-seq '[(ns hello.core)
;;                              (def a (for [i (range 10)]
;;                                       i))])))



(defmacro r! [name form deps]
  `(do
     (let [form# (quote ~form)
           deps# (quote ~deps)
           evaluable# (~'fn-evaluable (fn [~@deps] ~form)
                                      deps#)]
       (swap! ~'renv ~'env/set-form (quote ~name) evaluable# deps#)
       (~'env/shake! (deref ~'renv) (quote ~name)))
     
     (deref ~name)))
(defmacro defr [name form deps]
  `(do
     (defonce ~name (atom nil))
     (swap! ~'renv ~'env/set-ref (quote ~name) ~name)
     (~'r! ~name ~form ~deps)))


(defn javascript-environment [env]
  (let [compiler-env (env/default-compiler-env)
        ;; preamble (env/with-compiler-env compiler-env
        ;;            (vec (ana/forms-seq "/Users/adrian/workspace/minebot-clj/src/minebot_clj/environment.cljs")))
        ;; preamble2 (env/with-compiler-env compiler-env
        ;;             (vec (ana/forms-seq "/Users/adrian/workspace/minebot-clj/src/minebot_clj/evaluable.cljs")))
        env-def `[(~'ns ~'jsenv
                    (:require [minebot-clj.environment :as ~'env]
                              [minebot-clj.evaluable :refer [~'fn-evaluable ~'evaluate]])
                    (:require-macros [minebot-clj.celljs :refer [~'defr ~'r!]])
                    )
                  (def ~'renv (atom (~'env/environment)))
                  (declare ~@(-> env :deps keys))
                  ~@(for [name (keys (:refs env))]
                      `(do
                         (~'defr ~name ~(-> env :evaluables (get name) :form)
                           ~(-> env :deps (get name)))
                         (js/console.log ~(str name) @~name)))
                  ]
        js (cljsc/build
              (vec (concat ;;preamble
                           ;; preamble2
                           env-def))
              {:optimizations :whitespace :pretty-print true}
              compiler-env)
        ]
    ;;env-def
    (str
     "var console = {
    log: function(){
        var s = '';
        for (var i = 0; i < arguments.length;i ++){
            s += arguments[i] +' ' ;
        }
        print(s);
    }
};
"
     js)
   )
  ;; def environment
  ;; 
  )
