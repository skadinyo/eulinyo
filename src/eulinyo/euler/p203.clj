(ns eulinyo.euler.p203
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn put-one
  [coll]
  (vec (cons 1 (conj coll 1))))

(defn pascal-tri
  [n]
  (loop [c 1 coll [1] res []]
    (if (> c n)
      res
      (let [ncoll (->> (rest coll)
                       (mapv + coll)
                       put-one)]
        (recur (inc c) ncoll (conj res coll))))))