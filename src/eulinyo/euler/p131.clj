(ns eulinyo.euler.p131
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn problem-131-a
  [lim]
  "First try"
  (let [cube (fn [x] (*' x x x))
        cube-lim (->> (range)
                      (map vector (rest (range)))
                      (map (partial map cube))
                      (map (partial apply -))
                      (take-while #(> lim %))
                      count inc)
        primes (set (m/prime-sieve lim))]
    (->> (range)
         (map vector (range 1 cube-lim))
         (map (partial (comp (partial apply -)
                             (partial map cube))))
         set
         (cse/intersection primes)
         count)))

(defn problem-131-b
  "Optimizing A"
  [lim]
  (let [cube (fn [x] (* x x x))
        pre-prime (loop [j 2 temp []]
                    (let [i (dec j)
                          diff (- (cube j)
                                  (cube i))]
                      (if (> diff lim)
                        temp
                        (recur (inc j) (conj temp diff)))))]
    (count (filter m/prime? pre-prime))))

(defn problem-131-c
  "Sieve"
  [lim]
  (let [cube (fn [x] (* x x x))
        pre-prime (int-array lim)
        c-lim (loop [j 2]
                (let [i (dec j)
                      diff (- (cube j)
                              (cube i))]
                  (if (< diff lim)
                    (do (aset pre-prime i diff)
                        (recur (inc j)))
                    i)))]
    (->> (range 1 c-lim)
         (filter (fn [i]
                   (m/prime? (aget pre-prime i))))
         (count))))