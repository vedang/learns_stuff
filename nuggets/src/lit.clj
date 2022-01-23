(ns lit
  (:require [clojure.java.io :as jio]
            [clojure.string :as cs]
            [net.cgrand.enlive-html :as e]))

(defn- write-header
  "Extracts Header Information from the `scrape-data' and writes it
  into the `Writer' obj."
  [w ch-link ch-num]
  (let [link-line (format "#+Link: %s" ch-link)
        title-line (str "\n#+TITLE: Chapter " ch-num)]
    (.write w link-line)
    (.write w title-line)
    (.write w "\n")))

(defn- collect-pages
  "Given the list of page-number nodes, return the page numbers (ints)."
  [page-nodes]
  (->> page-nodes
       (filter :content)
       (filter (fn [el] (= "l_bJ" (get-in el [:attrs :class]))))
       last
       ((comp (fn [i] (if i (Integer/parseInt i) 0)) first :content))
       inc
       (range 2)))

(defn- clean-text
  "Clean the list of text-nodes, return a single string of scraped and
  formatted text."
  [ch-text-nodes]
  (apply str (interleave ch-text-nodes (repeat "\n\n"))))

(defn build-page
  "Given a `ch-link`, `page-num` and a string representing `existing-content`,
  return the content of the page appended to the existing content."
  [ch-link pg-num existing-content]
  (let [pg-link (str ch-link "?page=" pg-num)]
    (try (println "Scraping: " pg-link)
         (let [scrape-data (e/html-resource (java.net.URL. pg-link))
               page-content (-> scrape-data
                                (e/select [:div.aa_ht e/text-node])
                                clean-text)]
           (str existing-content "\n\n" page-content))
         (catch Exception e
           (println (format "Caught exception %s when scraping %s"
                            e
                            pg-link))))))

(defn build-chapter-file*
  "Function, without the exception handling part"
  [ch-num ch-link ch-filepath]
  (let [scrape-data (e/html-resource (java.net.URL. ch-link))
        first-page-content (-> scrape-data
                               (e/select [:div.aa_ht e/text-node])
                               clean-text)
        pages (-> scrape-data
                  (e/select [:a.l_bJ])
                  collect-pages)
        full-content (loop [content first-page-content
                            page-list pages]
                       (if (and (first page-list) content)
                         (recur (build-page ch-link
                                            (first page-list)
                                            content)
                                (rest page-list))
                         (if content
                           content
                           (throw (ex-info "No Content found!"
                                           {:ch ch-link
                                            :pages pages
                                            :curr-list page-list})))))]
    (with-open [^java.io.Writer w (jio/writer ch-filepath)]
      (write-header w ch-link ch-num)
      (.write w full-content))
    (println "Data written to: " ch-filepath)
    (inc ch-num)))

(defn build-chapter-file
  "Given a Chapter number and a Chapter link, write the chapter to a
  file. Return the link and number for the next chapter."
  [base-link base-path ch-num]
  (let [ch-link (apply str
                       base-link
                       (if (>= ch-num 10)
                         [ch-num]
                         ["0" ch-num]))
        ch-filepath (str base-path ch-num ".org")]
    (try (println "Scraping: " ch-link)
         (build-chapter-file* ch-num ch-link ch-filepath)
         (catch Exception e
           (println (format "Caught exception %s when scraping %s"
                            e
                            ch-link))))))

(defn- scrape-lit-impl
  [base-link base-path ch-num]
  (let [new-num (build-chapter-file base-link base-path ch-num)]
    (when new-num (recur base-link base-path new-num))))

(defn scrape-lit
  [base-link base-path ch-num]
  (println (format "[lit] Beginning to Scrape %s"
                   base-link))
  (.mkdirs (jio/as-file base-path))
  (scrape-lit-impl base-link base-path ch-num)
  (println "[lit] Done. Enjoy Reading."))

(defn- write-chapter-to-final-file
  [op ip]
  (let [ch-lines (line-seq ip)
        ch-title-line (second ch-lines)
        heading-line (apply str
                            "*"
                            (drop 8 ;; Drop the #+title: part
                                  ch-title-line))
        content (apply str (map (fn [s] (if (seq s) s "\n\n"))
                                (drop 2 ch-lines)))]
    (.write op (str "\n" heading-line))
    (.write op "\n")
    (.write op content)))


(defn- parse-chapter-file-name
  "Given a java.io.File chapter file, return the chapter number in
  Long format for chronological sorting."
  [ch-file]
  (try (Long/parseLong (first (cs/split (.getName ch-file) #"\.")))
       (catch NumberFormatException _ 0)))


(defn build-final-file
  "Assumes all scraping is done and chapter files are present in base
  path. Returns a single org file that can then be converted to HTML,
  PDF, Mobi etc."
  [base-path file-path story-name]
  (let [ch-files (sort (fn [f1 f2]
                         (compare (parse-chapter-file-name f1)
                                  (parse-chapter-file-name f2)))
                       (remove #(.isDirectory %)
                               (file-seq (java.io.File. base-path))))]
    (with-open [^java.io.Writer w (jio/writer file-path)]
      (.write w (format "\n#+TITLE: %s" story-name))
      (doseq [chf ch-files]
        (println "Adding Chapter: " (.getName chf))
        (with-open [^java.io.Reader r (jio/reader chf)]
          (write-chapter-to-final-file w r))))))

(comment
  ;; example usage
  (def base-link "test-link")
  (def base-path "resources/lit/chapters/")
  (.mkdirs (jio/as-file base-path))
  (def file-path "resources/lit/test_file.org")
  (def story-name "Test Story")
  (do (scrape-lit base-link base-path 1)
      (build-final-file base-path file-path story-name))
  ;; You can now convert the final worm.org file to whichever format you desire.
  )
