(ns eulinyo.euler.p188
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn b-exp
  [x n]
  (rem (cond
         (= n 0) 1
         (= n 1) x
         (even? n) (b-exp (rem (*' x x) 100000000) (quot n 2))
         :else (*' x (b-exp (rem (*' x x) 100000000) (quot (dec n) 2))))
       100000000))

(defn problem-188-a
  [x n]
  (if (= n 1)
    x
    (b-exp x (problem-188-a x (dec n)))))