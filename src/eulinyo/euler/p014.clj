(ns eulinyo.euler.p014
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(def next-move
  (memoize
    (fn [n]
      (if (even? n)
        (quot n 2)
        (inc (* 3 n))))))

(defn count-seq
  [x]
  (loop [i x c 0]
    (if (= i 1)
      (inc c)
      (recur (next-move i) (inc c)))))

(defn problem-14-a
  [lim]
  (loop [i 1 [c res] [0 0]]
    (if (> i lim)
      [c res]
      (let [c-seq (count-seq i)]
        (if (> res c-seq)
          (recur (inc i) [i res])
          (recur (inc i) [i c-seq]))))))

(defn go-on
  [x]
  (let [e (* 2 x)
        o (/ (dec x) 3)]
    (if (and (== o (int o))
             (> x 0))
      [e o]
      [e])))