(ns eulinyo.euler.p120
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn r-even
  [a]
  (* (- a 2) a))

(defn r-odd
  [a]
  (* (- a 1) a))

(defn r-max
  [a]
  (if (odd? a)
    (r-odd a)
    (r-even a)))

(defn problem-120-a
  [lim]
  (->> (range 3 (inc lim))
       (map r-max)
       (reduce +)))