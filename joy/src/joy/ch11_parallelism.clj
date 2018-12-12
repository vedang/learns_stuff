(ns joy.ch11-parallelism
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))

(defn feed->zipper
  "Given a RSS / Atom feed, fetch the data and create a zipper from it."
  [uri-str]
  (->> uri-str
       xml/parse
       zip/xml-zip))

(defn normalize
  "Convert both RSS feed and Atom feed zippers to the same structure."
  [feed]
  (if (= :feed (:tag (first feed)))
    feed
    (zip/down feed)))

(defn feed-children
  "Given a Feed URL, returns the elements of the feed that signify
  posts."
  [uri-str]
  (->> uri-str
       feed->zipper
       normalize
       zip/children
       (filter (comp #{:item :entry} :tag))))

(defn title
  "Given a feed entry, return the title of the entry if it's a Post.
  Else return nil."
  [entry]
  (some->> entry
           :content
           (some #(when (= :title (:tag %)) %))
           :content
           first))

(defn count-text-task
  "Given an `extractor` for getting text from feed items, a `txt` to
  match against, and a `feed` url, count the number of entries that
  match."
  [extractor txt feed]
  (let [items (feed-children feed)
        re    (re-pattern (str "(?i)" txt))]
    (->> items
         (map extractor)
         (mapcat (partial re-seq re))
         count)))

(def elixir-lang-feed "http://feeds.feedburner.com/ElixirLang")
(def feeds
  #{"http://feeds.feedburner.com/ElixirLang"
    "http://blog.fogus.me/feed/"})
