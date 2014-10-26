(defproject minebot-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.8"]
                 [org.apache.commons/commons-io "1.3.2"]
                 [org.clojure/core.cache "0.6.3"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.clojure/data.priority-map "0.0.4"]
                 [enlive "1.1.5"]
                 ;; [org.clojure/tools.analyzer "0.6.1"]
                 [org.clojure/tools.analyzer.jvm "0.6.1"]

                 [qtjambi/qtjambi "4.8.6"]
                 [qtjambi/qtjambi-macosx "4.8.6"]
                 ]
  :main minebot-clj.core)
