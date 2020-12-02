(ns aoc2020
  (:require [clojure.string :as cs]))

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
