(ns eulinyo.core
  (:require [clojure.set :as cse]
            [clojure.string :as cst]))

(defn time-me
  [f]
  (-> f
      (time)
      (with-out-str)
      (cst/split #"time: ")
      (last)
      (cst/split #" msecs")
      (first)
      (bigdec)))

(defn show-time
  ([f] (show-time f 10))
  ([f n]
   (let [times (for [i (repeat n 1)]
                 (time-me f))
         sorted-times (->> times
                           (sort))]
     {:max (last  sorted-times)
      :min (first sorted-times)
      :average (/ (apply + times) n)})))