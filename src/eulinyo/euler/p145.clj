(ns eulinyo.euler.p145
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn reversible-num?
  [x]
  (let [xs (m/number->collection x)
        y (m/collection->number (reverse xs))
        hs (m/number->collection (+ x y))]
    (if (= 0 (last xs))
      false
      (if (every? odd? hs)
        [x y]
        false))))

(defn problem-145-a
  [lim]
  (loop [i 1 res #{}]
    (if (> i lim)
      (count res)
      (recur (inc i)
             (if (res i)
               res
               (let [temp (reversible-num? i)]
                 (if temp
                   (apply conj res temp)
                   res)))))))