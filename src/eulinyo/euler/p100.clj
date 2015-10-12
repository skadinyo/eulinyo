(ns eulinyo.euler.p100
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn psquare?
  [x]
  (let [root (-> x Math/sqrt)]
    (== root (bigint root))))

(defn determinant
  [tot]
  (inc (*' 2 tot (dec tot))))

(defn blue-disk
  [tot]
  (let [det (determinant tot)]
    (if (psquare? det)
      (-> det
          Math/sqrt
          inc
          (quot 2))
      false)))

(defn problem-100-a
  [start]
  (loop [i (inc (bigint start))]
    (do
      (println i)
      (let [d (blue-disk i)]
        (if d [i (bigint d)] (recur (inc i)))))))


;; TODO Still wrong