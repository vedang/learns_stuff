(ns mr-money-mustache
  (:require [clj-time.core :as ct]
            [clojure.java.io :as jio]
            [clojure.string :as cs]
            [clojure.tools.logging :as ctl])
  (:import java.io.File
           [org.joda.time.format DateTimeFormat DateTimeFormatter]
           org.jsoup.Jsoup
           org.jsoup.nodes.TextNode))

(def archive-url
  "http://www.mrmoneymustache.com/all-the-posts-since-the-beginning-of-time/")
(def raw-data-dir "resources/mmm/posts/")
(def known-content? (atom true))

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
      fancy-space #" "
      ;; trick to preserve inserted \n across jsoup's .text function
      trick-nl #"\\n"
      ;; handle special chars to write good org files. * is useful to
      ;; link to subtrees in an org-file when used as follows:
      ;; [[*Foo][Link Text]]. In other cases, it is a headline
      ;; indicator and I don't want my content to have this char.
      org-special-char-is-not-subtree-link #"^(?!.*\[\*).*\*"
      org-special-char #"\*"]
  (def ^:private text-cleaner
    (comp #(cs/replace % fancy-single-quote "'")
          #(cs/replace % fancy-elipses "...")
          #(cs/replace % fancy-dash "--")
          #(cs/replace % fancy-double-quote "\"")
          #(cs/replace % fancy-space " ")
          #(cs/replace % trick-nl "\n")
          (fn [s]
            (if (re-find org-special-char-is-not-subtree-link s)
              (cs/replace s org-special-char "-")
              s))))
  (defn- clean-content
    [raw-content]
    (map text-cleaner raw-content)))


(defn- massage-single-a-elem
  [a-elem]
  (when (seq (.text a-elem))
    (if (= (.attr a-elem "href")
           (.text a-elem))
      ;; the href is the same as the link-text, keep only one.
      (do (.before a-elem (TextNode. (str "[[" (.attr a-elem "href") "]]") ""))
          (.empty a-elem))
      ;; form an org-mode link using the href and link-text
      (do (.before a-elem (TextNode. (str "[[" (.attr a-elem "href") "][") ""))
          (.after a-elem (TextNode. "]]" ""))))))


(defn- massage-brs-in-single-elem
  [elem]
  (doseq [e (.select elem "br")]
    (.after e (TextNode. "\\n" ""))))


(defn- massage-trs-in-single-elem
  [elem]
  (let [row-counter (atom 0)]
    (doseq [e (.select elem "tr")]
      (when (pos? @row-counter)
        (.before e (TextNode. (str @row-counter ". ") "")))
      (.after e (TextNode. "\\n" ""))
      (swap! row-counter inc))))


(defn- massage-ahrefs-in-single-elem
  [elem]
  (doseq [e (.select elem "a")]
    (massage-single-a-elem e)))


(defn- process-tables
  [table-elem content-strs]
  ;; first add a newline when you see a <tr>
  (massage-trs-in-single-elem table-elem)

  ;; now let's add this to the rest of our content
  (conj content-strs
        (str (.text table-elem)
             "\n")))


(defn- process-blockquotes
  [bq-elem content-strs]
  ;; first add a newline when you see a <br> or <tr>
  (massage-brs-in-single-elem bq-elem)
  (massage-trs-in-single-elem bq-elem)

  ;; next modify <a> links with text to org-link format
  (massage-ahrefs-in-single-elem bq-elem)

  ;; now let's add this to the rest of our content
  (conj content-strs
        (str "\n#+BEGIN_CENTER\n"
             (.text bq-elem)
             "\n#+END_CENTER\n")))

(defn- process-paragraphs
  [para-elem content-strs]
  ;; first add a newline when you see a <br> or <tr>
  (massage-brs-in-single-elem para-elem)
  (massage-trs-in-single-elem para-elem)

  ;; next modify <a> links with text to org-link format
  (massage-ahrefs-in-single-elem para-elem)

  ;; now let's add this to the rest of our content
  (if (seq (.text para-elem))
    (conj content-strs
          (str (.text para-elem) "\n\n"))
    content-strs))


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
             (.select list-element "li")))
    "\n")))


(defn- process-imgs
  [img-elem content-strs]
  (into content-strs
        ["\n#+BEGIN_EXAMPLE"
         (str "\nIMG: " (.attr img-elem "src"))
         (str "\nTITLE: " (.attr img-elem "title"))
         (str "\nALT: " (.attr img-elem "alt"))
         "\n#+END_EXAMPLE\n"]))


(defn- process-divs
  [div-element content-strs]
  (cond
    (.hasClass div-element "wp-caption")
    ;; handle image divs
    (if (and (first (.select div-element "> a"))
             (first (.select div-element "> p.wp-caption-text")))
      (into content-strs
            ["\n#+BEGIN_EXAMPLE"
             (str "\nIMG: "
                  (.attr (first (.select div-element "> a"))
                         "href"))
             (->> "> p.wp-caption-text"
                  (.select div-element)
                  first
                  .text
                  (str "\nCAPTION: "))
             "\n#+END_EXAMPLE\n"])
      content-strs)

    (or (.hasClass div-element "mmm-box")
        (.select div-element "div:not([class])"))
    (if (seq (.ownText div-element))
      ; div is not well-formed, all bets are off. Just let jsoup
      ; attempt a best case thing.
      (conj content-strs
            (str (.text div-element) "\n"))
      ; div is well-formed
      (extract-content-from-elements content-strs
                                     (.select div-element "> *")))

    :else (do (ctl/info "Unknown div class with content: "
                        (.html div-element))
              (reset! known-content? false)
              content-strs)))


(defn- process-basic-elem
  [elem content-strs]
  (if (seq (.text elem))
    (conj content-strs (.text elem))
    content-strs))


(defn- process-h-elem
  [indent-count h-elem content-strs]
  (if (seq (.text h-elem))
    (do (massage-ahrefs-in-single-elem h-elem)
        (conj content-strs (str "\n"
                                (apply str (repeat indent-count "-"))
                                " "
                                (.text h-elem)
                                "\n")))
    content-strs))


(defn- process-single-a-elem
  [a-elem content-strs]
  (massage-single-a-elem a-elem)
  (process-basic-elem a-elem content-strs))


(defn- extract-element-content
  [acc e]
  (case (.tagName e)
    "p" (process-paragraphs e acc)
    ("ol" "ul") (process-lists e acc)
    "li" (process-single-list-elem e acc)
    ("script" "hr" "br" "audio") acc ;; don't care
    "div" (process-divs e acc)
    ("h1" "h2" "h3" "h4" "h5") (-> e
                                   .tagName
                                   second
                                   str
                                   Integer/parseInt
                                   (process-h-elem e acc))
    ("em" "strong" "center" "i" "b" "dl") (process-basic-elem e acc)
    "a" (process-single-a-elem e acc)
    "blockquote" (process-blockquotes e acc)
    "table" (process-tables e acc)
    "img" (process-imgs e acc)
    (do (ctl/info (format "Unknown element type: %s\nContent: %s"
                         (.tagName e)
                         (.text e)))
        (reset! known-content? false)
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
                                  (.select post "div.post_content > *"))))


(defn- extract-post-header
  [post-url post]
  (let [author-line "#+AUTHOR: Mr. Money Mustache"
        title-line (str "\n#+TITLE: "
                        (-> post
                            (.select "h1.headline")
                            first
                            .text
                            text-cleaner))
        published-line (str "\n#+PUBLISHED: "
                            (-> post
                                (.select "span.post_date")
                                first
                                (.attr "title")))
        link-line (str "\n#+LINK: " post-url "\n\n")]
    [author-line title-line published-line link-line]))


(defn scrape-single-post
  [post-url]
  (ctl/info "\nScraping: " post-url)
  (reset! known-content? true)
  (let [post (.get (Jsoup/connect post-url))
        post-filename (make-post-filename post-url)
        post-header (extract-post-header post-url post)
        post-content (extract-post-content post)]
    (when @known-content?
      (write-to-file post-filename post-header post-content))))


(defn- get-all-post-links
  []
  (let [archive-page (.get (Jsoup/connect archive-url))
        blog-links (.select archive-page "ul.history > li > a[href]")]
    (reverse (map (fn [l] (.attr l "href")) blog-links))))


(defn- discard-scraped-posts
  [all-post-links]
  (let [all-filenames (map make-post-filename all-post-links)
        new-links+files (remove (fn [[l f]]
                                  (.exists (File. f)))
                                (partition 2
                                           (interleave all-post-links
                                                       all-filenames)))]
    (ctl/info (format "Discarding %s posts, since they're already scraped."
                      (- (count all-filenames)
                         (count new-links+files))))
    (map first new-links+files)))


(defn scrape-mmm
  "This is the main function. It will scrape any new posts it finds on
  Mr Money Mustache."
  []
  (let [all-post-links (get-all-post-links)
        new-post-links (discard-scraped-posts all-post-links)]
    (doseq [l new-post-links]
      (scrape-single-post l))))


(defn- add-proper-nls
  [l]
  (if (seq l) (str l "\n") "\n\n"))


(let [mustache-link-re #"http://www.mrmoneymustache.com/\d{4}/\d{2}/\d{2}/[^/]*"]
  (defn- replace-mustache-links-with-local-links
    [l]
    (if-let [mustache-links (re-seq mustache-link-re l)]
      (reduce (fn [acc ml]
                (let [local-link (make-post-filename ml)
                      local-title (with-open [rdr (jio/reader local-link)]
                                    (cs/replace-first (second (line-seq rdr))
                                                      "#+TITLE: "
                                                      ""))
                      org-local-link (str "*" local-title)]
                  (cs/replace acc ml org-local-link)))
              l
              (map #(str % "/")
                   mustache-links))
      l)))


(let [nl-counter (atom 0)]
  (defn- too-many-nls
    [l]
    (if (seq (cs/trim l))
      (do (reset! nl-counter 0)
          false)
      (do (swap! nl-counter inc)
          (if (> @nl-counter 1)
            (do (swap! nl-counter dec)
                true)
            false)))))


(defn- write-post-to-final-org-file
  [writer post]
  (try (with-open [^java.io.Reader rdr (jio/reader post)]
         (let [post-file-strs (reverse (drop-while (partial = "")
                                                   (reverse (line-seq rdr))))
               post-title-name (str "* "
                                    (cs/replace-first (second post-file-strs)
                                                      "#+TITLE: "
                                                      ""))
               post-content (apply str
                                   (map (comp replace-mustache-links-with-local-links
                                              add-proper-nls)
                                        (remove too-many-nls
                                                (drop 4 post-file-strs))))]
           (.write writer post-title-name)
           (.write writer post-content)
           (.write writer "\n")))
       (catch java.io.FileNotFoundException _
         ;; thrown if we're trying to read a dir
         (ctl/info "Caught: " post))))


(defn- get-post-published-date
  [post-file]
  (let [date-str (try (with-open [rdr (jio/reader post-file)]
                        (cs/replace-first (first (drop 2 (line-seq rdr)))
                                          "#+PUBLISHED: "
                                          ""))
                      (catch java.io.FileNotFoundException _
                        ;; thrown if we're trying to read a dir
                        "1970-01-01"))
        dtf ^DateTimeFormatter (DateTimeFormat/forPattern "yyyy-MM-dd")]
    (.parseDateTime dtf date-str)))


(defn build-final-org-file
  "All the posts are ready, let's go for it. Returns a single org file
  that can then be converted to HTML, PDF, Mobi etc."
  []
  (let [single-filepath (str raw-data-dir "../MrMoneyMustache.org")
        sorted-post-files (sort (fn [f1 f2]
                                  (compare (get-post-published-date f1)
                                           (get-post-published-date f2)))
                                (file-seq (java.io.File. raw-data-dir)))]
    (with-open [^java.io.Writer w (jio/writer single-filepath)]
      (.write w "#+AUTHOR: Mr. Money Mustache")
      (.write w "\n#+TITLE: Mr. Money Mustache's Blog Posts")
      (.write w (str "\n#+CREATED_AT: " (ct/now) "\n"))
      (doseq [f sorted-post-files]
        (ctl/info "Adding Post: " (.getName f))
        (write-post-to-final-org-file w f)))))
