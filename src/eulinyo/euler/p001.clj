(ns eulinyo.euler.p001
  (:require [eulinyo.core :refer [show-time]]
            [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defmacro divisible?
  [a b]
  `(= 0 (rem ~a ~b)))

(defn problem-1-a
  [lim]
  (->> (range 1 lim)
       (filter (fn [x] (or (divisible? x 3)
                           (divisible? x 5))))
       (apply +)))

(defn problem-1-b
  [lim]
  (->> [(range 3 lim 3)  (range 5 lim 5)]
       (map set)
       (apply cse/union)
       (apply +)))