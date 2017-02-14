(ns mr-money-mustache
  (:require [clojure.java.io :as jio]
            [clojure.string :as cs]
            [clojure.tools.logging :as ctl])
  (:import java.io.File
           org.jsoup.Jsoup
           org.jsoup.nodes.TextNode))

(def archive-url
  "http://www.mrmoneymustache.com/all-the-posts-since-the-beginning-of-time/")
(def raw-data-dir "resources/mmm/posts/")


(defn- get-jsoup-doc-from-url
  [url]
  (.get (Jsoup/connect url)))


(defn- get-jsoup-elements
  [obj css-selector]
  (.select obj css-selector))


(defn- make-post-filename
  [url]
  (str raw-data-dir
       (cs/join "-"
                (drop 3
                      (cs/split url #"/")))
       ".org"))


(defn- write-to-file
  [filename header-strs content-strs]
  (ctl/info "Writing to " filename)
  (with-open [^java.io.Writer fd (jio/writer filename)]
    (doseq [s header-strs]
      (.write fd s))
    (doseq [s content-strs]
      (.write fd s))))


(declare extract-content-from-elements)


(let [fancy-double-quote #"“|”|″"
      fancy-single-quote #"’|‘"
      fancy-elipses #"…"
      fancy-dash #"–|—"
      fancy-space #" "]
  (def ^:private text-cleaner
    (comp #(cs/replace % fancy-single-quote "'")
          #(cs/replace % fancy-elipses "...")
          #(cs/replace % fancy-dash "--")
          #(cs/replace % fancy-double-quote "\"")
          #(cs/replace % fancy-space " ")))
  (defn- clean-content
    [raw-content]
    (map text-cleaner raw-content)))


(defn- process-paragraphs
  [para-element content-strs]
  ;; first add a newline when you see a <br>
  (doseq [e (.select para-element "br")]
    (.after e (TextNode. "\\n" "")))

  ;; next modify <a> links with text to org-link format
  (doseq [e (.select para-element "a")]
    (when (seq (.text e))
      (if (= (.attr e "href")
             (.text e))
        ;; the href is the same as the link-text, keep only one.
        (do (.before e (TextNode. (str "[[" (.attr e "href") "]]") ""))
            (.empty e))
        ;; form an org-mode link using the href and link-text
        (do (.before e (TextNode. (str "[[" (.attr e "href") "][") ""))
            (.after e (TextNode. "]]" ""))))))

  ;; now let's add this to the rest of our content
  (conj content-strs
        (str ;; trick to preserve \n across jsoup's .text function
             (cs/replace (.text para-element)
                         "\\n"
                         "\n")
             "\n\n")))

(defn- process-single-list-elem
  ([list-elem content-strs]
   (process-single-list-elem list-elem "--" content-strs))
  ([list-elem list-marker content-strs]
   (conj content-strs
         (str "  "
              list-marker
              " "
              (.text list-elem)
              "\n"))))


(defn- process-lists
  ([list-element content-strs]
   (process-lists list-element content-strs 1))
  ([list-element content-strs list-num]
   (conj
    (first
     (reduce (fn [[acc eli-count] eli]
               [(process-single-list-elem eli (str eli-count ".") acc)
                (inc eli-count)])
             [content-strs list-num]
             (get-jsoup-elements list-element "li")))
    "\n")))


(defn- process-divs
  [div-element content-strs]
  (cond
    (.hasClass div-element "wp-caption")
    (do ;; ignore image divs
      (ctl/info "Ignoring image div with content: "
                (.html div-element))
      content-strs)

    (or (.hasClass div-element "mmm-box")
        (.select div-element "div:not([class])"))
    (extract-content-from-elements (if (seq (.ownText div-element))
                                     (conj content-strs
                                           (str (.ownText div-element) "\n"))
                                     content-strs)
                                   (get-jsoup-elements div-element "> *"))


    :else (do (ctl/info "Unknown div class with content: "
                       (.html div-element))
              content-strs)))


(defn- extract-element-content
  [acc e]
  (case (.tagName e)
    "p" (process-paragraphs e acc)
    ("ol" "ul") (process-lists e acc)
    "li" (process-single-list-elem e acc)
    "script" acc ; don't care
    "div" (process-divs e acc)
    (do (ctl/info (format "Unknown element type: %s\nContent: %s"
                         (.tagName e)
                         (.text e)))
        acc)))


(defn- extract-content-from-elements
  [content-collector elements]
  (reduce extract-element-content
          content-collector
          elements))


(defn- extract-post-content
  [post]
  (clean-content
   (extract-content-from-elements []
                                  (get-jsoup-elements post
                                                      "div.post_content > *"))))


(defn- extract-post-header
  [post-url post]
  (let [author-line "#+AUTHOR: Mr. Money Mustache"
        title-line (str "\n#+TITLE: "
                        (-> post
                            (get-jsoup-elements "h1.headline")
                            first
                            .text
                            text-cleaner))
        published-line (str "\n#+PUBLISHED: "
                            (-> post
                                (get-jsoup-elements "span.post_date")
                                first
                                (.attr "title")))
        link-line (str "\n#+LINK: " post-url "\n\n")]
    [author-line title-line published-line link-line]))


(defn- scrape-single-post
  [post-url]
  (ctl/info "\nScraping: " post-url)
  (let [post (get-jsoup-doc-from-url post-url)
        post-filename (make-post-filename post-url)
        post-header (extract-post-header post-url post)
        post-content (extract-post-content post)]
    (write-to-file post-filename post-header post-content)))


(defn- get-all-post-links
  []
  (let [archive-page (get-jsoup-doc-from-url archive-url)
        blog-links (get-jsoup-elements archive-page
                                       "ul.history > li > a[href]")]
    (reverse (map (fn [l] (.attr l "href")) blog-links))))


(defn- discard-scraped-posts
  [all-post-links]
  (let [all-filenames (map make-post-filename all-post-links)]
    (map second
         (remove (fn [[f l]]
                   (.exists (File. f)))
                 (partition 2
                            (interleave all-filenames all-post-links))))))


(defn scrape-mmm
  "This is the main function. It will scrape any new posts it finds on
  Mr Money Mustache."
  []
  (let [all-post-links (get-all-post-links)
        new-post-links (discard-scraped-posts all-post-links)]
    (doseq [l new-post-links]
      (scrape-single-post l))))
