(ns worm
  "A scraper for the online web-series - Worm (ParaHumans).

  The author has discouraged the distribution of ebooks. From his FAQ:

      There's unethical sorts who are taking others' work and claiming
      it as their own (often with a title/name change) and I'd rather
      not make it easier for them.  I know there's a few fanmade ebook
      versions circulating, I accept that it'll happen, but I don't
      want to help the process along.

  In keeping with his wishes, this scraper simply scrapes all the
  chapters from the site and creates an org file for easy reading. The
  process of making the ebook - albeit straightforward - is left to
  the reader.

  ;; example usage
  (do (scrape-worm 1 base-link)
      (build-final-file))
  ;; You can now convert the final worm.org file to whichever format you desire.
  "
  (:require [net.cgrand.enlive-html :as e]
            [clojure.java.io :as jio]
            [clojure.string :as cs]))


(def base-path "target/chapters/")
(def base-link "http://parahumans.wordpress.com/2011/06/11/1-1/")


(defn- clean-text
  "Clean the scraped text."
  [ch-text]
  (let [;; Remove the irritating &nbsp; char
        remove-nbsp (map #(cs/replace % #" " "") ch-text)
        add-extra-new-lines (map #(cs/replace % #"\n" "\n\n") remove-nbsp)
        ;; Remove "Next Chapter" "Last Chapter" and all other useless text
        trim-beginning (drop-while #(<= (count (cs/trim %)) 12) add-extra-new-lines)
        trim-end (reverse (drop-while #(<= (count (cs/trim %)) 12)
                                      (reverse trim-beginning)))]
    (apply str trim-end)))


(defn- write-header
  "Extracts Header Information from the `scrape-data' and writes it
  into the `Writer' obj."
  [w ch-num scrape-data]
  (let [author-line "#+AUTHOR: wildbow"
        title-line (str "\n#+TITLE: Worm - Chapter "
                        ch-num
                        " - "
                        (e/text
                         (first (e/select scrape-data [:h1.entry-title]))))
        published-line (str "\n#+PUBLISHED: "
                            (first (e/attr-values
                                    (first (e/select scrape-data [:.entry-date]))
                                    :datetime)))
        link-line (str "\n#+LINK: "
                       (first (e/attr-values
                               (first (e/select scrape-data [:.entry-meta (e/attr-contains :rel "bookmark")]))
                               :href))
                       "\n\n")]
    (.write w author-line)
    (.write w title-line)
    (.write w published-line)
    (.write w link-line)))


(defn build-chapter-files
  "Given a Chapter number and a Chapter link, write the chapter to a
  file. Return the link and number for the next chapter."
  [ch-num ch-link]
  (println "Scraping: " ch-link)
  (let [ch-filepath (str base-path ch-num ".org")
        scrape-data (e/html-resource (java.net.URL. ch-link))
        node-content (clean-text (e/select scrape-data
                                           [:.entry-content e/text-node]))
        next-chapter-link (first (e/attr-values
                                  (first (filter #(= "Next Chapter"
                                                     (cs/trim (e/text %)))
                                                 (e/select scrape-data
                                                           [:.entry-content :a])))
                                  :href))]
    (with-open [^java.io.Writer w (jio/writer ch-filepath)]
      (write-header w ch-num scrape-data)
      (.write w node-content))
    (println "Data written to: " ch-filepath)
    [(inc ch-num) next-chapter-link]))


(defn- scrape-worm*
  [ch-num ch-link]
  (let [[new-num new-link] (build-chapter-files ch-num ch-link)]
    (when new-link (recur new-num new-link))))


(defn scrape-worm
  [ch-num ch-link]
  (println "Beginning to Scrape")
  (.mkdirs (jio/as-file base-path))
  (scrape-worm* ch-num ch-link)
  (println "Done. Enjoy Reading."))


(defn- write-chapter-to-final-file
  [op ip]
  (let [ch-lines (line-seq ip)
        ch-title-line (second ch-lines)
        heading-line (apply str
                            "*"
                            (cs/join "-"
                                     (drop 1
                                           (cs/split ch-title-line
                                                     #"-"))))
        heading-content (apply str (map (fn [s] (if (seq s) s "\n\n"))
                                        (drop 4 ch-lines)))]
    (.write op (str "\n" heading-line))
    (.write op heading-content)))


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
  []
  (let [single-filepath (str base-path "../worm.org")
        ch-files (drop 1
                       (sort (fn [f1 f2]
                               (compare (parse-chapter-file-name f1)
                                        (parse-chapter-file-name f2)))
                             (file-seq (java.io.File. base-path))))]
    (with-open [^java.io.Writer w (jio/writer single-filepath)]
      (.write w "#+AUTHOR: wildbow")
      (.write w "\n#+TITLE: Worm - Parahumans")
      (doseq [chf ch-files]
        (println "Adding Chapter: " (.getName chf))
        (with-open [^java.io.Reader r (jio/reader chf)]
          (write-chapter-to-final-file w r))))))


(comment
  ;; example usage
  (do (scrape-worm 1 base-link)
      (build-final-file))
  ;; You can now convert the final worm.org file to whichever format you desire.
  )
