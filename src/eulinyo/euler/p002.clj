(ns eulinyo.euler.p002
  (:require [eulinyo.core :refer [show-time]]
            [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn problem-2-a
  [lim]
  (-> (loop [i1 1 i2 2 res 0]
        (if (> i1 lim)
          res
          (if (even? i1)
            (recur i2 (+ i1 i2) (+ res i1))
            (recur i2 (+ i1 i2) res))))
      time))