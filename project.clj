(defproject minebot-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 ;; [org.clojure/clojure "1.5.1"]

                 [clj-http "0.7.8"]
                 [org.apache.commons/commons-io "1.3.2"]
                 [org.clojure/core.cache "0.6.3"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.clojure/data.priority-map "0.0.4"]
                 [enlive "1.1.5"]
                 ;; [org.clojure/tools.analyzer "0.6.1"]
                 [org.clojure/tools.analyzer.jvm "0.6.1"]
                 [org.marianoguerra/clj-rhino "0.2.1"]
                 [org.clojars.houshuang/keymaster-clj "0.1.0"]
                 [qtjambi/qtjambi "4.8.6"]
                 [qtjambi/qtjambi-macosx "4.8.6"]
                 [hiccup-bootstrap "0.1.2"]
                 [org.clojure/data.json "0.2.5"]

                 [jtidy "4aug2000r7-dev"]
                 [org.clojure/clojurescript "0.0-2371"]

                 [org.python/jython-standalone "2.7-b3"]
                 [clojure-python "0.4.1" :exclusions [org.clojure/clojure
                                                      org.python/jython-standalone]]

                 [phronmophobic/penumbra "0.6.6-SNAPSHOT"]

                 ]
  :profiles {:dev {:plugins [[com.keminglabs/cljx "0.4.0"]]}}
  :cljx {:builds [{:source-paths ["src"]
                   :output-path "src"
                   :rules :clj}
                  {:source-paths ["src"]
                   :output-path "src"
                   :rules :cljs}]}
  :main minebot-clj.core)
