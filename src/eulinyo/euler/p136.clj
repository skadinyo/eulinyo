(ns eulinyo.euler.p136
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn problem-136-a
  [lim]
  (let [temp (int-array (inc lim))]
    (loop [u 1]
      (when (< u lim)
        (loop [v 1]
          (let [uv (* u v)
                v3 (* v 3)]
            (when (<= uv lim)
              (when (and (> v3 u)
                         (= 0 (rem (- v3 u) 4))
                         (= 0 (rem (+ u v) 4)))
                (aset temp uv (inc (aget temp uv))))
              (recur (inc v)))))
        (recur (inc u))))
    (->> (range 1 lim)
         (filter #(= 1 (aget temp %)))
         (count))))