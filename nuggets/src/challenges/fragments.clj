(ns challenges.fragments
  "Issued by: Neha
   Date: <2016-04-09 Sat>
   Problem Statement: Given broken fragments of sentences, piece them
   together."
  (:require [clojure.string :as cs]))


(defn merge-overlapping-fragments
  "Given two fragments where the second fragment overlaps with the
  first, return the merged fragment. If the second fragment does not
  overlap with the first, return `nil`."
  [^String frag1 ^String frag2 & [existing-index]]
  (when-let [first-char-index (if existing-index
                                (cs/index-of frag1
                                             (first frag2)
                                             (inc existing-index))
                                (cs/index-of frag1 (first frag2)))]
    (let [frag-match (subs frag1 first-char-index)]
      (if (cs/starts-with? frag2 frag-match)
        (str frag1 (subs frag2 (count frag-match)))
        (merge-overlapping-fragments frag1 frag2 first-char-index)))))


(defn merge-if-possible
  "Given two fragments, merge them into a single fragment if it is
  possible. Else return nil."
  [^String frag1 ^String frag2]
  (or (merge-overlapping-fragments frag1 frag2)
      (merge-overlapping-fragments frag2 frag1)
      (when (cs/includes? frag1 frag2) frag1)
      (when (cs/includes? frag2 frag1) frag2)))


(def merge-non-overlapping-fragments
  "Since the fragments are non-overlapping, we can merge them in any
  way we want."
  str)


(defn stitch-fragment-if-possible
  "Given a string and a bunch of fragments to stitch it with,
  return the stitched-fragment-string and all the other unstitched
  fragments."
  [stitched-string-so-far & fragments]
  (reduce (fn [[s unstitched-frags] frag]
            (if-let [stitched-string (merge-if-possible s frag)]
              [stitched-string unstitched-frags]
              [s (conj unstitched-frags frag)]))
          [stitched-string-so-far []]
          fragments))


(defn stitch
  [& fragments]
  (let [[stitched-string unstitched-frags] (apply stitch-fragment-if-possible
                                                  (first fragments)
                                                  (rest fragments))]
    (loop [loop-counter (count unstitched-frags) ; This is the maximum
                                                 ; number of times we
                                                 ; need to cycle
                                                 ; through the input
                                                 ; to try and stitch
                                                 ; it together.
           stitched-string stitched-string
           unstitched-frags unstitched-frags]
      (if (and (pos? loop-counter) (seq unstitched-frags))
        (let [[new-stitched-string new-unstitched-frags]
              (apply stitch-fragment-if-possible
                     (first unstitched-frags)
                     (conj (rest unstitched-frags) stitched-string))]
          (recur (dec loop-counter) new-stitched-string new-unstitched-frags))
        (apply merge-non-overlapping-fragments stitched-string unstitched-frags)))))
