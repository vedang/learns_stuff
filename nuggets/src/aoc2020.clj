(ns aoc2020
  (:require [clojure.java.io :as io]
            [clojure.set :as cset]
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
  "Given a `batch-file`, and a function to process all the lines in
  the file, return the result of processing the file."
  [batch-file batch-fn]
  (with-open [rdr (io/reader (io/resource batch-file))]
    (doall (batch-fn (line-seq rdr)))))

(comment
  (count (filter (partial s/valid? ::north-pole-passport)
                 (read-batch-file "aoc/test-input-4.txt" batch-lines->objs))))


(defn seat-num
  "Given a seat-str, bounds, lower-char and upper-char, find the
  seat-num using binary spacing."
  [seat-str bounds lower-char upper-char]
  (first
   (reduce (fn [[s e] c]
             (let [mid (/ (inc (- e s)) 2)]
               (cond
                 (= c lower-char) [s (- e mid)]
                 (= c upper-char) [(+ s mid) e])))
           bounds
           seat-str)))

(defn seat-id
  "Given a seat-str, get the row-num, col-num and return
  (+ col-num (* 8 row-num))"
  [seat-str]
  (+ (seat-num (subs seat-str 7) [0 7] \L \R)
     (* 8 (seat-num (subs seat-str 0 7) [0 127] \F \B))))

(comment (apply max (map seat-id input-5))
         (reduce (fn [a b]
                   (if (not= (inc a) b)
                     (reduced [a b])
                     b))
                 (sort (map seat-id input-5))))

(defn group-ans-processor
  [batch-lines group-ans-processor-fn]
  (let [[final-ans last-group]
        (reduce (fn [[ans curr-group] line]
                  (if (empty? line)
                    [(+ ans (count (apply group-ans-processor-fn curr-group))) []]
                    [ans (conj curr-group (into #{} line))]))
                [0 []]
                batch-lines)]
    (+ final-ans (count (apply group-ans-processor-fn last-group)))))

(comment
  (read-batch-file "aoc/test-input-6.txt" #(group-ans-processor % cset/union))
  (read-batch-file "aoc/test-input-6.txt" #(group-ans-processor % cset/intersection)))

(defn process-single-bag-rule
  "Given a rule like \"light blue bags contain 1 green bag, 3 white
  bags.\", return a vertex -> edges representation [light blue -> [1
  green], [3 white]]."
  [bag-rule]
  (let [[bag-color contains] (cs/split bag-rule #" bags contain ")]
    [bag-color
     (keep identity
           (map (comp (fn [[snum & scolors]]
                        (try [(Integer/parseInt snum) (cs/join " " scolors)]
                             (catch Exception _)))
                      butlast
                      (fn [s] (cs/split s #" ")))
                (cs/split contains #", |\.")))]))

(defn bags-reverse-mapper
  "Given all the rules representing which bag can contain which other
  bags, return a reverse mapping of bag1 -- can be contained by
  --> [bag2, bag3]."
  [bag-rules]
  (reduce (fn [bag-map bag-rule]
            (let [[bag-color bag-edges] (process-single-bag-rule bag-rule)]
              (reduce (fn [bag-map [_ bag-c]]
                        (update bag-map bag-c conj bag-color))
                      bag-map
                      bag-edges)))
          {}
          bag-rules))

(defn color-containers
  "Given a `bag-map` generated by `bags-reverse-mapper`, return all the
  bags that could contain a bag of your chosen `color`."
  ([bag-map color]
   (disj (color-containers bag-map #{} color) color))
  ([reverse-bag-map container-set color]
   (if (reverse-bag-map color)
     ;; down the rabbit hole to check if any of these bags can hide
     ;; inside other colored bags.
     (apply cset/union
            (map (partial color-containers
                          reverse-bag-map
                          (conj container-set color))
                 (reverse-bag-map color)))
     ;; this bag can't be put inside any other colored bag, our search
     ;; ends with this color.
     (conj container-set color))))

(defn bags-graph
  "Given all the rules representing which bag can contain which other
  bags, return a weighted graph representation {V -> [[W E]]}."
  [bag-rules]
  (into {} (map process-single-bag-rule bag-rules)))

(defn count-your-bags
  "Given a `bag-map` generated by `bags-graph` and a bag of some
  `color`, return the total number of bags that have been packed
  inside it. Note that the number returned counts the parent-bag as
  well."
  [bag-map parent-bag-color]
  (reduce (fn [total [weight color]]
            (+ total (* weight (count-your-bags bag-map color))))
          1
          (bag-map parent-bag-color)))

(comment
  (-> "aoc/test-input-7.txt"
      (read-batch-file bags-reverse-mapper)
      (color-containers "shiny gold")
      count)
  (-> "aoc/test-input-7-2.txt"
      (read-batch-file bags-graph)
      (count-your-bags "shiny gold")
      dec))

(defn rotate
  [coll n]
  (assert (> (count coll) (Math/abs n)))
  (if (neg? n)
    (rotate coll (+ (count coll) n))
    (concat (drop n coll) (take n coll))))

(defn boot-inst-processor
  "Given a boot instruction like nop +0, return [nop 0]."
  [inst]
  (let [[op v] (cs/split inst #" ")
        vint (Integer/parseInt v)]
    [op vint]))

(defn boot-code-debugger
  ([boot-code]
   (let [indexed-boot-code (map-indexed (fn [i c] [i c]) boot-code)]
     (boot-code-debugger indexed-boot-code
                         0
                         #{}
                         (last indexed-boot-code)
                         false)))
  ([boot-code accumulator seen-instructions last-instruction finished?]
   (cond
     ;; Am I done?
     finished? [accumulator ::exit]

     ;; Hello boot-code, my old friend!
     (seen-instructions (first boot-code))
     [accumulator ::loop]

     ;; Run tiny interpreter
     :else
     (let [bc (first boot-code)
           [_ inst] bc
           [op vint] (boot-inst-processor inst)]
       (case op
         "acc" (recur (rotate boot-code 1)
                      (+ accumulator vint)
                      (conj seen-instructions bc)
                      last-instruction
                      (= last-instruction bc))
         "nop" (recur (rotate boot-code 1)
                      accumulator
                      (conj seen-instructions bc)
                      last-instruction
                      (= last-instruction bc))
         "jmp" (recur (rotate boot-code vint)
                      accumulator
                      (conj seen-instructions bc)
                      last-instruction
                      (= last-instruction bc)))))))

(defn- replace-inst-and-debug
  "In the given `boot-code`, replace the inst at `inst-counter`
  location with the `new-inst`. Run the debugger and check if we
  exit.

  This is a helper function and only runs inside `boot-code-fixer`."
  [boot-code inst-counter new-inst]
  (let [[acc reason] (boot-code-debugger
                      (concat (take inst-counter boot-code)
                              (list new-inst)
                              (drop (inc inst-counter) boot-code)))]
    (if (= ::exit reason)
      ;; Found the fix!
      (reduced [acc reason])
      (inc inst-counter))))

(defn boot-code-fixer
  "Fix one single jmp or nop instruction to check if the program exits.
  Logic: Change nops where offset is positive (to jmp), change jmps
  where offset is negative (to nop) and try."
  [boot-code]
  (reduce (fn [inst-counter inst]
            (let [[op vint] (boot-inst-processor inst)]
              (cond
                ;; Change pos nop to jmp and try
                (and (= op "nop") (pos? vint))
                (replace-inst-and-debug boot-code
                                        inst-counter
                                        (str "jmp " vint))

                ;; Change neg jmp to nop and try
                (and (= op "jmp") (neg? vint))
                (replace-inst-and-debug boot-code
                                        inst-counter
                                        (str "nop " vint))

                :else (inc inst-counter))))
          0
          boot-code))

;;; Day 9

(defn xmas-corrupt-data-detector
  "Given the `corrupt-data` and the length of the preamble `p-len`,
  find the first num in the data post the preamble which is not a sum
  of 2 numbers in the preceding `p-len` numbers."
  [corrupt-data p-len]
  (let [data-under-test (take p-len corrupt-data)
        item (first (drop p-len corrupt-data))]
    (if-let [pair (find-nums-with-sum item data-under-test)]
      (do ;; (println (format "%s is the sum of : %s" item pair))
          (recur (concat (drop 1 corrupt-data) (take 1 corrupt-data)) p-len))
      item)))

(defn sum-till-greater-than
  [weak-n data]
  )

(defn xmas-weakness-detector
  "Given the weak number `weak-n`, and the `data`, return a contiguous
  list of numbers whose sum is `weak-n`."
  ;; idea: sum nums till sum exceeds weak-n, then drop initial numbs
  ;; till sum falls under weak-n. At any point, if the sum is exactly
  ;; the same, that list is the answer.
  [data weak-n]
  (let [[res sum contiguous-data] (sum-till-greater-than weak-n data)]
    (case res
      ::exact contiguous-data
      ::exceeds (drop-till-less-than weak-n sum data))))
