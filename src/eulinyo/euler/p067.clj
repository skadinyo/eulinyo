(ns eulinyo.euler.p067
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn triangle
  []
  (->> (slurp "./resources/problem/p067.txt")
       (cst/split-lines)
       (map #(->> (cst/split % #" ")
                   (map bigint)))))

(defn problem-67-a
  [triangle]
  (first (reduce (fn [x next]
                   (let [x-> (rest x)
                         x-min (map max x x->)]
                     (map + x-min next)))
                 (reverse triangle))))

(defn problem-67-b
  [triangle]
  (loop [[x y & xs] (reverse triangle)]
    (if xs
      (recur (cons (->> (map + (map max x (rest x)) y)) xs))
      (first x))))

(defn problem-67-c
  [triangle]
  (first (reduce (fn [x next]
                   (let []
                     (map (fn [a b c]
                            (+ c (max a b))) x (rest x) next)))
                 (reverse triangle))))

(defn problem-67-d
  [triangle]
  (loop [[x y & xs] (reverse triangle)]
    (if xs
      (recur (cons  (map (fn [a b c] (+ c (max a b))) x (rest x) y)
                   xs))
      (first x))))
