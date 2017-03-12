(ns joy.ch10-mutation-and-concurrency-locks
  (:refer-clojure :exclude [aget aset count seq])
  (:require [clojure.core :as clj]
            [joy.ch10-mutation-and-concurrency :refer (dothreads!)])
  (:import [java.util.concurrent.locks ReentrantLock ReentrantReadWriteLock]))


(defprotocol SafeArray
  (aset  [this i f])
  (aget  [this i])
  (count [this])
  (seq   [this]))

(defn make-dumb-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq   [_] (clj/seq a))
      (aget  [_ i] (clj/aget a i))
      (aset  [this i f]
        (clj/aset a i
                  (f (aget this i)))))))

(defn pummel [a]
  (dothreads! #(dotimes [i (count a)] (aset a i inc))
              :threads 100))

(comment
  (def D (make-dumb-array Integer/TYPE 8))
  (pummel D)
  (seq D))

(defn make-safe-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq   [_] (clj/seq a))
      (aget  [_ i] (clj/aget a i))
      (aset  [this i f]
        (locking a
          (clj/aset a i
                    (f (aget this i))))))))

(comment
  (def A (make-safe-array Integer/TYPE 8))
  (pummel A)
  (seq A))

(defn lock-i
  [target-index num-locks]
  (mod target-index num-locks))

(defn make-smart-array [t sz]
  (let [a (make-array t sz)
        lsz (/ sz 2)
        L (into-array (take lsz (repeatedly #(ReentrantLock.))))]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq   [_] (clj/seq a))
      (aget  [_ i]
        (let [lk (clj/aget L (lock-i (inc i) lsz))]
          (.lock lk)
          (try (clj/aget a i)
               (finally (.unlock lk)))))
      (aset  [this i f]
        (let [lk (clj/aget L (lock-i (inc i) lsz))]
          (.lock lk)
          (try (clj/aset a i
                         (f (aget this i)))
               (finally (.unlock lk))))))))

(defn make-smart2-array [t sz]
  (let [a (make-array t sz)
        lsz (/ sz 2)
        L (into-array (take lsz (repeatedly #(ReentrantReadWriteLock.))))]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq   [_] (clj/seq a))
      (aget  [_ i]
        (let [rlk (clj/aget L (lock-i (inc i) lsz))]
          (.lock (.readLock rlk))
          (try (clj/aget a i)
               (finally (.unlock (.readLock rlk))))))
      (aset  [this i f]
        (let [wlk (clj/aget L (lock-i (inc i) lsz))]
          (.lock (.writeLock wlk))
          (try (clj/aset a i
                         (f (aget this i)))
               (finally (.unlock (.writeLock wlk)))))))))
