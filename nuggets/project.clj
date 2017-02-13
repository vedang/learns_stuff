(defproject nuggets "1.0.0"
  :description "tasty nuggets"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [enlive "1.1.6" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.apache.logging.log4j/log4j-api "2.8"]
                 [org.apache.logging.log4j/log4j-core "2.8"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.8"]
                 [com.fasterxml.jackson.core/jackson-core "2.8.5"]
                 [com.fasterxml.jackson.core/jackson-databind "2.8.5"]
                 [com.fasterxml.jackson.core/jackson-annotations "2.8.5"]
                 [org.jsoup/jsoup "1.10.2"]])
