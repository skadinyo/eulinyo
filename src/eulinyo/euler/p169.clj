(ns eulinyo.euler.p169
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn problem-169-a
  [n]
  (if (zero? n)
    [1 0]
    (let [[a b] (problem-169-a (quot n 2))]
      (if (even? n)
        [(+ a b) b]
        [a (+ a b)]))))
