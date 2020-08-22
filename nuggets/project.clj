(defproject nuggets "1.0.0"
  :description "tasty nuggets"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [enlive "1.1.6" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.logging "0.6.0"]
                 [org.apache.logging.log4j/log4j-api "2.13.0"]
                 [org.apache.logging.log4j/log4j-core "2.13.0"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.13.0"]
                 [com.fasterxml.jackson.core/jackson-core "2.10.2"]
                 [com.fasterxml.jackson.core/jackson-databind "2.10.2"]
                 [com.fasterxml.jackson.core/jackson-annotations "2.10.2"]
                 [org.clojure/core.specs.alpha "0.2.44"]
                 [org.jsoup/jsoup "1.12.2"]
                 [clj-time "0.15.2"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/test.check "1.0.0"]
                 [org.clojure/core.async "1.2.603"]])
