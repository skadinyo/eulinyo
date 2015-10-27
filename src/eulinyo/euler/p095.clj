(ns eulinyo.euler.p095
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn doseq-sopd
  [lim]
  (let [sopd (int-array lim 1)]
    (do
      (doseq [i (range 2 (-> lim Math/sqrt int inc))]
        (doseq [j (range (* i 2) lim i)]
          (aset sopd j (+ (aget sopd j)
                          (if (== (* i i) j)
                            i
                            (+ i (quot j i)))))))
      (vec sopd))))

(defn loop-sopd
  [lim]
  (let [d (int-array (inc lim) 1)
        root-lim (Math/sqrt lim)]
    (loop [i 2]
      (when (<= i root-lim)
        (loop [j (+ i i)]
          (when (<= j lim)
            (aset d j (+ (aget d j)
                         (if (== (* i i) j) i (+ i (quot j i)))))
            (recur (+ j i))))
        (recur (inc i))))
    (vec d)))

(defn problem-95-a
  [lim]
  (let [d (zipmap (range 1 lim) (rest (read-string (slurp "./resources/proper-divisor"))))
        find-chain (fn [x]
                     (loop [[start & chains] [x] i (get d x)]
                       (if (= start i)
                         (cons start chains)
                         (if (or (nil? i)
                                 (some #{i} chains))
                           false
                           (recur (cons start (concat chains [i]))
                                  (get d i))))))]
    (->> (range 1 lim)
         (pmap find-chain)
         (filter identity)
         (map set)
         (set)
         (vec)
         (sort-by count)
         (last))))