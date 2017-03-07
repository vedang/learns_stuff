(ns joy.ch05-collection-types)


(defn neighbours
  ([matrix-size yx-pair]
   (neighbours [[-1 0] [1 0] [0 -1] [0 1]]
               matrix-size
               yx-pair))
  ([deltas matrix-size yx-pair]
   (filter (fn [new-yx-pair]
             (every? #(< -1 % matrix-size) new-yx-pair))
           (map #(vec (map + yx-pair %))
                deltas))))
