(ns eulinyo.euler.p029
 (:require [eulinyo.math :as m]
           [clojure.set :as cse]
           [clojure.string :as cst]))

(defn problem-29-a
  [limit]
  (time
   (loop [i 2 power 2 res #{} c 0]
     (if (> i limit)
       (println c)
       (if (> power limit)
         (recur (inc i) 2 res c)
         (let [ipow (m/pow i power)]
           (if (some #{ipow} res)
             (recur i (inc power) res c)
             (recur i (inc power) (conj res ipow) (inc c)))))))))

(defn problem-29-b
  [limit]
  (let [is (range 2 (inc limit))]
    (->> is
         (map (fn [i]
                (map vector (repeat i) is)))
         (apply concat)
         (map (partial apply m/pow))
         (distinct)
         (count))))

(comment 
  (problem-29-a 100)
  (problem-29-b 100))
