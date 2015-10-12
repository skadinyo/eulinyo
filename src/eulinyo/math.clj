(ns eulinyo.math)

(defn range-ratio
  ([start ratio]
   (cons start (lazy-seq (range-ratio (*' start ratio) ratio))))
  ([start end ratio]
   (if (> start end)
     '()
     (cons start (lazy-seq (range-ratio (*' start ratio) end ratio))))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn count-number
  [x]
  (if (< x 10)
    1
    (+ 1 (count-number (quot x 10)))))

(defn prime?
  [x]
  (cond
    (or (= 2 x)
        (= 3 x)) true
    (or (> 2 x)
        (= 0 (rem x 2))
        (= 0 (rem x 3))) false
    :else (let [lim (int (Math/sqrt x))]
            (loop [i 5]
              (if (> i lim)
                true
                (if (= 0 (rem x i))
                  false
                  (recur (+' i 2))))))))

(defn next-prime
  [x]
  (if (= x 1)
    2
    (loop [i (if (even? x)
               (+ x 1)
               (+ x 2))]
      (if (prime? i)
        i
        (recur (+ i 2))))))

(defn prev-prime
  [x]
  (if (<= x 3)
    2
    (loop [i (if (even? x)
                 (- x 1)
                 (- x 2))]
        (if (prime? i)
          i
          (recur (+ i 2))))))

(defn prime-sieve
  [lim]
  (let [res (boolean-array (inc lim) true)
        par (int (Math/sqrt lim))]
    (do
      (doseq [i (range 2 lim)
              :while (<= i par)
              :when (aget res i)]
        (doseq [j (range (+ i i) lim i)]
          (aset res j false)))
      (filter #(aget res %)
              (range 2 lim)))))

(defn totient-sieve
  [lim]
  (let [p (set (prime-sieve lim))
        tot (int-array (range 0 lim))]
    (do
      (doseq [ip p]
        (doseq [i (range (* 2 ip) lim ip)]
          (aset tot i (int (* (aget tot i)
                              (- 1 (/ 1 ip)))))))
      (map (fn [i]
             (if (p i)
               [i (dec i)]
               [i (aget tot i)]))
           (range 1 lim)))))

(defn divrem
  [x n]
  [(quot x n) (rem x n)])

(defn number->collection
  "Convert a number into a list"
  [x]
  (if (< x 10)
    [x]
    (conj (number->collection (quot x 10)) (rem x 10))))

(defn collection->number
  [coll]
  (->> (reverse coll)
       (map * (range-ratio 1 10))
       (apply +)))

(defn pow
  "a^b"
  [a b]
  (reduce *' (repeat b a)))

(defn count-div
  [a b]
  (loop [temp a res 0]
    (let [[d r] (divrem temp b)]
      (if (= 0 r)
        (recur d (inc res))
        res))))

(defn div-until
  [a b]
  (let [[d r] (divrem a b)]
    (if (zero? r)
      (div-until d b)
      a)))

(defn psquare?
  [n]
  (let [sn (Math/sqrt n)]
    (== sn (int sn))))
