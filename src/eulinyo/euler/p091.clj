(ns eulinyo.euler.p091
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]
            [clojure.math.combinatorics :as combo]))

(defn absolute
  [x]
  (if (> x 0)
    x
    (* x -1)))

(defn distance
  [[x1 y1] [x2 y2]]
  (let [dx (absolute (- x1 x2))
        dy (absolute (- y1 y2))]
    (Math/sqrt (+ (m/pow dx 2)
                  (m/pow dy 2)))))

(def m-distance (memoize distance))

(defn combi-3
  [p1 p2 p3]
  [[p1 p2] [p1 p3] [p2 p3]])

(defn phythagoras
  [points]
  (let [combi (apply combi-3 points)
        [a b c] (->> combi
                     (map (partial apply m-distance))
                     (sort))]
    (== c (Math/sqrt (+ (m/pow a 2)
                        (m/pow b 2))))))

(def m-phytagoras
  (memoize phythagoras))

(defn problem-91-a
  [lim]
  (let [r (range 0 (inc lim))
        points (for [x r
                     y r]
                 [x y])]
    (->> (for [i1 (rest points)
               i2 (rest points)
               :when (not= i1 i2)]
           [i1 i2])
         (pmap (partial cons [0 0]))
         (pmap set)
         (set)
         (filter m-phytagoras)
         (count))))

;; TODO STILL FAILED