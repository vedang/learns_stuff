(ns little-cljer.ch03-cons-the-magnificent)
;;; Or in our case, `conj`.

;;; Now, `conj` behaves differently than `cons`.
;;; `conj` is optimized for the various collection types that it takes
;;; Therefore:

;;; <code>
;;; (conj '(:a) :b) => '(:b :a) ; append to beginning of list
;;; (conj [:a] :b) => [:a :b] ; append to end of vector
;;; </code>

;;; *Note*:
;;; Since `cons` always appends to the beginning of a list in scheme,
;;; we will be strictly using lists to represent collections in this
;;; chapter.


;;; > The Second Commandment
;;; > ----------------------
;;; > Use `cons` to build lists.


(defn rember
  "Remove the _first_ occurrence of atom `a` from list `lat`"
  [a lat]
  (cond
   (empty? lat) '()
   (= a (first lat)) (rest lat)
   :else (conj (rember a (rest lat)) (first lat))))


(defn firsts
  "Return the first element of each list in the list of lists `ll`"
  [ll]
  (cond
   (empty? ll) '()
   :else (conj (firsts (rest ll)) (first (first ll)))))


;;; > The Third Commandment
;;; > ---------------------
;;; > When building a list, describe the first typical element, and then
;;; > `cons` it onto the natural recursion.


(defn insertR
  "Take atoms `new` and `old`, along with a list `lat` and insert new
  to the right of the first occurrence of old."
  [new old lat]
  (cond
   (empty? lat) '()
   (= old (first lat)) (conj (conj (rest lat) new) old)
   :else (conj (insertR new old (rest lat)) (first lat))))


(defn insertL
  "Take atoms `new` and `old`, along with a list `lat` and insert new
  to the left of the first occurrence of old."
  [new old lat]
  (cond
   (empty? lat) '()
   (= old (first lat)) (conj lat new)
   :else (conj (insertL new old (rest lat)) (first lat))))


(defn subst
  "Take atoms `new` and `old`, along with a list `lat` and replace
  first occurrence of old with new."
  [new old lat]
  (cond
   (empty? lat) '()
   (= old (first lat)) (conj (rest lat) new)
   :else (conj (subst new old (rest lat)) (first lat))))


;;; Go cons a piece of cake onto your mouth.


(defn subst2
  "Replace either the first occurrence of `o1` or the first occurrence
  of `o2` with `new` in `lat`"
  [new o1 o2 lat]
  (cond
   (empty? lat) '()
   (or (= o1 (first lat))
       (= o2 (first lat))) (conj (rest lat) new)
   :else (conj (subst2 new o1 o2 (rest lat)) (first lat))))


;;; Go repeat the cake-consing


(defn multirember
  "Remove all occurrences of `a` from `lat`"
  [a lat]
  (cond
   (empty? lat) '()
   (= a (first lat)) (multirember a (rest lat))
   :else (conj (multirember a (rest lat)) (first lat))))


(defn multiinsertR
  "Insert `new` to the right of `old` for all instances of `old` in `lat`"
  [new old lat]
  (cond
   (empty? lat) '()
   (= old (first lat)) (conj (conj (multiinsertR new old (rest lat))
                                   new)
                             old)
   :else (conj (multiinsertR new old (rest lat)) (first lat))))


(defn multiinsertL
  "Insert `new` to the left of `old` for all instances of `old` in `lat`"
  [new old lat]
  (cond
   (empty? lat) '()
   (= old (first lat)) (conj (conj (multiinsertL new old (rest lat))
                                   old)
                             new)
   :else (conj (multiinsertL new old (rest lat)) (first lat))))


;;; > The Fourth Commandment
;;; > ----------------------
;;; > Always change at least one argument when recurring. It must be
;;; > changed to be closer to termination. The changing argument must be
;;; > tested in the termination condition.


(defn multisubst
  "Replace all occurrences of `old` with `new` in `lat`"
  [new old lat]
  (cond
   (empty? lat) '()
   (= old (first lat)) (conj (multisubst new old (rest lat)) new)
   :else (conj (multisubst new old (rest lat)) (first lat))))
