(ns eulinyo.euler.p123
  (:require [eulinyo.math :as m
             :refer [pow]]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn r
  [n a]
  (* 2 a n))

(defn problem-123-a
  [lim]
  (let [primes (->> (m/prime-sieve 10000000)
                    (map vector (rest (range))))]
    (->> primes
         (filter #(odd? (first %)))
         (drop-while #(<= (apply r %) lim))
         (first)
         ((juxt identity (partial apply r))))))

(defn r-cal
  [n a]
  (rem (+ (pow (dec a) n) (pow (inc a) n))
       (* a a)))