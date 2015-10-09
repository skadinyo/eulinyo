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
      (Double/parseDouble)))
