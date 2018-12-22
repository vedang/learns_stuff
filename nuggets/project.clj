(defproject nuggets "1.0.0"
  :description "tasty nuggets"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [enlive "1.1.6" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.logging "0.4.1"]
                 [org.apache.logging.log4j/log4j-api "2.11.1"]
                 [org.apache.logging.log4j/log4j-core "2.11.1"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.11.1"]
                 [com.fasterxml.jackson.core/jackson-core "2.9.7"]
                 [com.fasterxml.jackson.core/jackson-databind "2.9.7"]
                 [com.fasterxml.jackson.core/jackson-annotations "2.9.7"]
                 [org.jsoup/jsoup "1.11.3"]
                 [clj-time "0.15.1"]])
