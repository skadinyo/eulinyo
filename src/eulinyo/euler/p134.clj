(ns eulinyo.euler.p134
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn find-pair
  [[[p1 c] p2]]
  (println p1 p2)
  (loop [x p2]
    (let [l (rem x c)]
      (if (= p1 l)
        [p1 p2 x]
        (recur (+ x p2))))))

(defn problem-134
  []
  (let [primes (->> (m/prime-sieve 1100000)
                    (drop-while (partial > 5)))
        p1s (->> primes
                 (take-while (partial > 1000000))
                 (map (fn [x]
                        [x (m/pow 10 (m/count-number x))])))
        pairs (map vector p1s (rest primes))]
    (->> pairs
         (pmap find-pair)
         (map last)
         (reduce +'))))

(comment (time (problem-134)))