(ns eulinyo.euler.p051
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn repeating-digits
  [x]
  (let [s (str x)
        [k v] (->> (frequencies s)
               (sort-by last)
               last)]
    [(int (bigint (str k))) v x]))

(defn replace-k
  [k coll]
  (loop [[x & xs] coll res (repeat (- 10 k) [])]
    (if x
      (recur xs (map #(conj %1 %2) res (if (= x k)
                                         (range k 10)
                                         (repeat x))))
      (->> res
           (map (partial apply str))
           (map bigint)
           (map int)))))

(defn problem-51-a
  []
  (let [primes (->> (m/prime-sieve 1000000)
                    (drop-while (partial > 99999)))
        temp (->> primes
                  (map repeating-digits)
                  (filter #(and (> 3 (first %))
                                (= 3 (second %)))))]
    (->> temp
         (map (fn [[k v coll]]
                (replace-k k (m/number->collection coll))))
         (map (fn [coll]
                (filter m/prime? coll)))
         (filter #(= 8 (count %)))
         first)))

(comment (take 200 (problem-51-a)))