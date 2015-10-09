(ns eulinyo.euler.p003
  (:require [eulinyo.core :refer [deft]]
            [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn problem-3-a
  [x]
  (-> (loop [temp x prime 2 res 0]
        (if (<= temp 1)
          res
          (if (= 0 (rem temp prime))
            (recur (m/div-until temp prime)
                   (m/next-prime prime)
                   prime)
            (recur temp
                   (m/next-prime prime)
                   res))))))