(ns eulinyo.core
  (:require [clojure.set :as cse]
            [clojure.string :as cst]
            [clj-time.core :as t]
            [clj-time.coerce :as c]))

(defn time-me
  [f]
  (with-out-str (time f)))

(defn show-time
  ([f] (show-time f 1000))
  ([f n]
   (let [tete (fn []
                (time-me f))
         times (repeatedly n tete)
         sorted-times (->> times
                           (sort))]
     {:max (last  sorted-times)
      :min (first sorted-times)
      :average (/ (apply + times) n)})))