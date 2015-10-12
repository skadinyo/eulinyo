(ns eulinyo.euler.p125
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn sum-of-squares
  [n]
  (quot (* n (+ n 1) (+ (* 2 n) 1)) 6))

(defn palin?
  [x]
  (let [coll (m/number->collection x)]
    (= coll (reverse coll))))


(defn problem-125-a
  [lim]
  (let [dada (->> (range 1 (-> lim Math/sqrt int inc))
                  (map sum-of-squares))]
    (->> (for [i dada]
           (->> dada
                (map #(- % i))
                (filter (partial < 0))
                (rest)))
         (cons (rest dada))
         (apply concat)
         (filter (partial > lim))
         (distinct)
         (filter palin?)
         ((juxt count (partial reduce +) sort)))))

(defn problem-125-b
  [lim]
  (let [squares (->> (range 1 (-> lim Math/sqrt int inc))
                     (map sum-of-squares))
        a-lim (count squares)
        sum-array (int-array squares)]
    (->> (loop [c 2
                [s & ss] squares
                temp (-> squares rest set)]
           (if s
             (recur (inc c)
                    ss
                    (let [is (range c a-lim)
                          ns (->> is
                                  (map (partial aget sum-array))
                                  (map #(-' % s))
                                  (filter (partial > lim)))]
                      (cse/union temp (set ns))))
             temp))
         (filter palin?)
         (reduce +'))))

;; TODO 125-b, still wrong ?....
;; TODO Finished, but still to slow.