(ns challenges.recur-in-try)
;;; Issued by: Sidhant
;;; Date: <2013-06-18 Tue>
;;; Recur does not work in try-catch blocks. Work around it.


(defn test-fn
  "This function should run indefinitely inside a try catch.
  Ignore the divide-by-zero exceptions."
  []
  (let [res (/ (rand-int 10) (rand-int 10))]
    (println res)
    res))


(defn run-fn
  "My wrapper to run a given function indefinitely in a future while
  ignoring exceptions thrown by it."
  [f & {:keys [exit-val]
        :or {exit-val nil}}]
  (future (loop [continue true]
            (let [res (try (f)
                           (catch Throwable t
                             (println "Throws " t)))]
              (if (= res exit-val) ;; Added for testing quickly on the REPL
                res
                (recur true))))))
