(defproject nuggets "1.0.0"
  :description "tasty nuggets"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-time "0.15.2"]
                 [clojure.java-time "0.3.3"]
                 [com.brunobonacci/mulog "0.8.1"]
                 [com.brunobonacci/mulog-json "0.8.1"]
                 [com.brunobonacci/mulog-zipkin "0.8.1"]
                 [enlive "1.1.6" :exclusions [org.clojure/clojure]]
                 [hbs "1.0.3"]
                 [hiccup "1.0.5"]
                 [instaparse "1.4.10"]
                 [org.clojure/clojure "1.10.3"]
                 [org.clojure/core.async "1.5.648"]
                 [org.clojure/test.check "1.1.1"]
                 ;; Upgrading to 1.14 has breaking changes that need
                 ;; to be fixed first.
                 [org.jsoup/jsoup "1.13.1"]])
