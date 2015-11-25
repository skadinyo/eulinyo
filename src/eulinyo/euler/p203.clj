(ns eulinyo.euler.p203
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn put-one
  [coll]
  (vec (cons 1 (conj coll 1))))

(defn pascal-tri
  [n]
  (loop [c 1 coll [1] res []]
    (if (> c n)
      res
      (let [ncoll (->> (rest coll)
                       (mapv + coll)
                       put-one)]
        (recur (inc c) ncoll (conj res coll))))))

(defn problem-203-a
  []
  (let [pascals (->> (pascal-tri 51)
                     (apply concat)
                     distinct)
        max-p (->> (apply max pascals)
                   Math/sqrt int inc)
        primes (->> max-p
                    (m/prime-sieve)
                    (map #(*' % %))
                    vec)
        sqf? (fn [x]
               (loop [[i & is] primes]
                 (if i
                   (if (< x i)
                     true
                     (if (= 0 (rem x i))
                       false
                       (recur is)))
                   true)))]
    (->> pascals
         (filter sqf?)
         (reduce +))))