(ns aoc2020
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as cs]))

(defn find-nums-with-sum
  "Given a number `sum` and a list of numbers `nums`, find 2 numbers
  in `nums` which sum up to `sum`."
  [sum nums]
  (let [res (reduce (fn [seen-num? curr]
                      (let [target (- sum curr)]
                        (if (seen-num? target)
                          (reduced [curr target])
                          (conj seen-num? curr))))
                    #{}
                    nums)]
    (when-not (set? res) res)))

;;; I know that this solution does not handle repeats / same numbers.
;;; I'm okay with it. I have captured this in the tests
(defn find-three-nums-with-sum
  "Given a number `sum` and a list of numbers `nums`, find 3 numbers
  in `nums` which sum up to `sum`."
  [sum nums]
  (reduce (fn [_ curr]
            (let [target (- sum curr)]
              (when-let [[num1 num2] (find-nums-with-sum target nums)]
                (reduced [curr num1 num2]))))
          nil
          nums))

(defn valid-password?
  "Returns true if the `password` is according to the `policy`. `policy`
  is [`pchar` `pmin` `pmax`], meaning that `pchar` should appear in the
  `password` at least `pmin` number of times and no more than `pmax`
  number of times."
  [policy password]
  (let [[pchar pmin pmax] policy]
    (<= pmin (count (filter (partial = pchar) password)) pmax)))

(defn policy-input->policy
  "Convert the `input` string to a `policy` we understand."
  [input]
  (let [pchar (last input)
        [minstr maxcharstr] (cs/split input #"-")
        pmin (Integer/parseInt minstr)
        pmax (Integer/parseInt (first (cs/split maxcharstr #" ")))]
    [pchar pmin pmax]))

(defn input-string->password-policy
  "Convert the `input` string to a `policy` and `password` we can process."
  [input]
  (let [[policy-str password] (cs/split input #":")
        policy (policy-input->policy policy-str)]
    [policy (cs/triml password)]))

(comment
  ;; Number of valid passwords in the DB:
  (count (filter identity
                 (map (comp (partial apply valid-password?)
                            input-string->password-policy)
                      '("2-4 r: prrmspx"
                        "5-6 p: hpzplphxb"
                        "5-8 t: ttttbtttttc"
                        "1-6 k: kkkkkk"
                        "1-3 q: qqqq")))))

(defn valid-password-new-rule?
  "Returns true if the `password` is according to the `policy`. `policy`
  is [`pchar` `pmin` `pmax`], meaning that `pchar` should appear in the
  `password` at position `pmin` OR at position `pmax` (but not both)."
  [policy password]
  (let [[pchar pmin pmax] policy
        pmin-char (nth password (dec pmin))
        pmax-char (nth password (dec pmax))]
    (or (and (= pmin-char pchar)
             (not= pmax-char pchar))
        (and (not= pmin-char pchar)
             (= pmax-char pchar)))))

(defn tree-coords
  "Given an input, return the tree co-ords in that input."
  [input]
  (mapcat (fn [[item rownum]]
            (keep identity
                  (map-indexed (fn [idx it]
                                 (when (= \# it)
                                   ;; return [x, y] co-ordinates
                                   [idx rownum]))
                               item)))
          (partition 2 (interleave input (range)))))

(defn calc-movement
  [start-pos slope-pos max-pos]
  (let [new-pos (rem (+ start-pos slope-pos) max-pos)]
    (if (neg? new-pos)
      (+ max-pos new-pos)
      new-pos)))

(defn slope-points
  "Given a starting co-ordinate (x,y = pos), a slope co-ordinate (x,y =
  motion along x,y axis), and max co-ordinate (x,y = total num of
  cols, total num of rows), return all the points on the slope I will
  hit until I return to x = starting co-ordinate."
  [[start-x start-y] [slope-x slope-y] [count-x count-y]]
  (loop [curr-x start-x
         curr-y start-y
         prev-y start-y
         points [[start-x start-y]]]
    (let [moved-x (calc-movement curr-x slope-x count-x)
          moved-y (calc-movement curr-y slope-y count-y)]
      (if (> prev-y moved-y) ;; we rolled over on the y axis
        points
        (recur moved-x moved-y curr-y (conj points [moved-x moved-y]))))))

(comment
  (count
   (clojure.set/intersection (set (tree-coords input-3))
                             (set (slope-points [0 0] [3 1] [11 11])))))

(defn drop-every
  "Given an `input` seq and a `num` of elems to drop, return a sequence
  starting from first element and then dropping every `num` elements.
  eg: (drop-every [1 2 3 4 5] 1) => [1 3 5]"
  [drop-num input]
  (lazy-seq
   (when (seq input)
     (cons (first input)
           (drop-every drop-num (drop (inc drop-num) input))))))

(defn count-trees-in-path
  "Given input paths and movement instructions, return the number of
  trees you see when travelling along the path. Each path is
  represented as a collection of . and #. # == tree. We have to go
  through all the paths, starting from the top, every y-paths."
  [input-paths x-movement-coord y-movement-coord]
  (let [max-x (count (first input-paths))]
    (first
     (reduce (fn [[tree-count curr-x] line]
               (if (= \# (nth line (rem (+ curr-x x-movement-coord) max-x)))
                 [(inc tree-count) (+ curr-x x-movement-coord)]
                 [tree-count (+ curr-x x-movement-coord)]))
             [0 0]
             ;; We always start with the yth line, since that's where
             ;; we start looking for trees. From that point on, we
             ;; want to look at only every yth line in the input.
             ;; (down-1 movement means we don't want to drop any line,
             ;; down-2 means we want to drop every alternate line and
             ;; so on)
             (drop-every (dec y-movement-coord)
                         (drop y-movement-coord input-paths))))))

(defn valid-yr?
  [yr min-yr max-yr]
  (and (string? yr)
       (= 4 (count yr))
       (try (<= min-yr (Integer/parseInt yr) max-yr)
            (catch Exception _))))

(defn valid-hgt?
  [hgt]
  (let [l (cs/split hgt #"cm|in")]
    (and (empty? (rest l))
         (cond
           (cs/ends-with? hgt "cm")
           (try (<= 150 (Integer/parseInt (first l)) 193)
                (catch Exception _))

           (cs/ends-with? hgt "in")
           (try (<= 59 (Integer/parseInt (first l)) 76)
                (catch Exception _))

           :else false))))

(defn valid-hcl?
  [hcl]
  (re-matches #"^#[0-9a-f]{6}$" hcl)
  (and (= 7 (count hcl))
       (cs/starts-with? hcl "#")
       ()))

(s/def ::byr #(valid-yr? % 1920 2002)) ;; byr (Birth Year) 1937
(s/def ::iyr #(valid-yr? % 2010 2020)) ;; iyr (Issue Year) 2017
(s/def ::eyr #(valid-yr? % 2020 2030)) ;; eyr (Expiration Year) 2020
(s/def ::hgt valid-hgt?) ;; hgt (Height) 183cm
(s/def ::hcl (partial re-matches #"^#[0-9a-f]{6}$")) ;; hcl (Hair Color) #fffffd
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}) ;; ecl (Eye Color) gry
(s/def ::pid (partial re-matches #"^[0-9]{9}$")) ;; pid (Passport ID) 860033327
(s/def ::cid string?) ;; cid (Country ID) 147
(s/def ::passport
  (s/keys :req [::byr
                ::iyr
                ::eyr
                ::hgt
                ::hcl
                ::ecl
                ::pid
                ::cid]))
(s/def ::north-pole-passport
  (s/keys :req [::byr
                ::iyr
                ::eyr
                ::hgt
                ::hcl
                ::ecl
                ::pid]
          :opt [::cid]))

(defn process-line
  "Given a `line` in the batch-file, return an object of properties on
  the line.
  eg line looks like this: ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  output:
  {::ecl \"gry\"
   ::pid \"860033327\"
   ::eyr \"2020\"
   ::hcl \"#fffffd\"}"
  [line]
  (into {}
        (map (fn [i]
               (let [[k v] (cs/split i #":")]
                 [(keyword (str *ns*) k) v]))
             (cs/split line  #" "))))

(defn batch-lines->objs
  [batch-lines]
  (let [[os co]
        (reduce (fn [[objs curr-obj] line]
                  (if (empty? line)
                    [(conj objs curr-obj) {}]
                    [objs (merge curr-obj (process-line line))]))
                [[] {}]
                batch-lines)]
    (conj os co)))

(defn read-batch-file
  "Given a `batch-file`, return all the objects in the batch."
  [batch-file]
  (with-open [rdr (io/reader (io/resource batch-file))]
    (batch-lines->objs (line-seq rdr))))

(comment
  (count (filter (partial s/valid? ::passport)
                 (read-batch-file "aoc/test-input-4.txt"))))
