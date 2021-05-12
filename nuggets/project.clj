(defproject nuggets "1.0.0"
  :description "tasty nuggets"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.2"]
                 [enlive "1.1.6" :exclusions [org.clojure/clojure]]
                 [com.brunobonacci/mulog "0.6.4"]
                 [com.brunobonacci/mulog-zipkin "0.6.4"]
                 [com.brunobonacci/mulog-json "0.6.4"]
                 [org.jsoup/jsoup "1.13.1"]
                 [clj-time "0.15.2"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/test.check "1.1.0"]
                 [org.clojure/core.async "1.3.610"]
                 [instaparse "1.4.10"]])
