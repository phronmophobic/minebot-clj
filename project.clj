(defproject minebot-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [;;[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojure "1.7.0-beta2"]
                 ;; [org.clojure/clojure "1.5.1"]

                 [clj-http "0.7.8"]
                 [org.apache.commons/commons-io "1.3.2"]
                 [org.clojure/core.cache "0.6.3"]

                 ;; update to the latest core async version
                 ;; that i couldn't find on clojars.
                 ;; no actual changes of my own. will replace
                 ;; once clojars version gets updated
                 [com.phronemophobic/core.async "0.1.0-SNAPSHOT"]
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

                 [clj-pdf "2.0.2"]

                 [com.taoensso/faraday "1.5.0"]
                 [clj-time "0.9.0"]
                 [environ "1.0.0"]
                 [org.clojure/algo.graph "0.1.0-SNAPSHOT"]

                 [phronmophobic/penumbra "0.6.6-SNAPSHOT"]

                 [http-kit "2.1.11"]
                 [compojure "1.1.6"]
                 [ring-mock "0.1.5"]
                 [javax.servlet/servlet-api "2.5"]

                 
                 ]
  ;; :java-cmd "/Library/Java/JavaVirtualMachines/jdk1.7.0_45.jdk/Contents/Home/bin/java"
  :jvm-opts ["-XX:MaxPermSize=128m"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSClassUnloadingEnabled"]
  
  :profiles {:dev {:plugins [[com.kemingblabs/cljx "0.4.0"]]}}
  :plugins [[lein-environ "1.0.0"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
    :cljx {:builds [{:source-paths ["src"]
                   :output-path "src"
                   :rules :clj}
                  {:source-paths ["src"]
                   :output-path "src"
                   :rules :cljs}]}
  :main minebot-clj.core)
