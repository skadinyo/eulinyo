(ns eulinyo.euler.p035
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]
            [clojure.core.reducers :as r]))

(defn move-back
  [[x & xs]]
  (-> (vec xs)
      (conj x)))

(defn circular-prime?
  [x]
  (if (< x 10)
    true
    (let [xs (m/number->collection x)]
      (if (some #{2 4 5 6 8 0} xs)
        false
        (let [c (count xs)]
          (->> (iterate move-back xs)
               (take c)
               (map m/collection->number)
               (every? m/prime?)))))))

(defn problem-35-a
  [lim]
  (r/reduce (fn [c i]
              (if (circular-prime? i)
                (inc c)
                c))
            0 (m/prime-sieve lim)))