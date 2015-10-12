(ns eulinyo.euler.p004
  (:require [eulinyo.core :refer [show-time]]
            [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn palin?
  [x]
  (let [coll (m/number->collection x)]
    (= coll (reverse coll))))

(defn change-to
  [x]
  (fn [_] x))
