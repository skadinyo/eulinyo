(ns eulinyo.euler.p345
  (:require [eulinyo.math :as m]
            [clojure.set :as cse]
            [clojure.string :as cst]))

(defn matrix
  []
  (->> (-> "https://projecteuler.net/problem=345"
           slurp (cst/split #"<p style=\"text-align:center;font-family:'courier new';\">")
           last (cst/split #"</p>")
           first (cst/split-lines)
           rest)
       (map (comp first #(cst/split % #"<br />")))
       (mapv (comp (partial mapv (comp int bigint))
                   (partial map (partial re-find #"\d+"))
                   #(cst/split % #" ")))))

(defn test-matrix
  []
  [[  7  53 183 439 863]
   [497 383 563  79 973]
   [287  63 343 169 583]
   [627 343 773 959 943]
   [767 473 103 699 303]])

[[  7  53 183 439 863 497 383 563  79 973 287  63 343 169 583]
 [627 343 773 959 943 767 473 103 699 303 957 703 583 639 913]
 [447 283 463  29  23 487 463 993 119 883 327 493 423 159 743]
 [217 623   3 399 853 407 103 983  89 463 290 516 212 462 350]
 [960 376 682 962 300 780 486 502 912 800 250 346 172 812 350]
 [870 456 192 162 593 473 915  45 989 873 823 965 425 329 803]
 [973 965 905 919 133 673 665 235 509 613 673 815 165 992 326]
 [322 148 972 962 286 255 941 541 265 323 925 281 601  95 973]
 [445 721  11 525 473  65 511 164 138 672  18 428 154 448 848]
 [414 456 310 312 798 104 566 520 302 248 694 976 430 392 198]
 [184 829 373 181 631 101 969 613 840 740 778 458 284 760 390]
 [821 461 843 513  17 901 711 993 293 157 274  94 192 156 574]
 [ 34 124   4 878 450 476 712 914 838 669 875 299 823 329 699]
 [815 559 813 459 522 788 168 586 966 232 308 833 251 631 107]
 [813 883 451 509 615  77 281 613 459 205 380 274 302  35 805]]

[[-964 -920 -789 -534  -94 -384 -590 -410 -894    0 -684 -910 -539 -804 -390]
 [-330 -616 -185    0    0 -100 -486 -856 -260 -656    0 -256 -285 -320  -46]
 [-544 -710 -529 -964 -954 -414 -530    0 -874 -110 -664 -500 -479 -834 -250]
 [-764 -360 -979 -584 -114 -484 -880    0 -894 -520 -691 -467 -680 -521 -633]
 [   0 -586 -279    0 -646  -90 -476 -460  -50 -162 -710 -616 -699 -150 -612]
 [-117 -533 -796 -827 -380 -424  -74 -944    0 -116 -164  -24 -473 -660 -186]
 [ -17  -27  -86  -73 -843 -227 -327 -757 -483 -379 -317 -177 -736    0 -666]
 [-649 -825    0  -11 -671 -626  -32 -432 -708 -650  -46 -692 -281 -878    0]
 [-401 -127 -836 -323 -359 -691 -337 -684 -710 -176 -828 -420 -603 -400    0]
 [-560 -520 -665 -664 -162 -780 -410 -456 -674 -728 -280    0 -455 -584 -778]
 [-783 -140 -595 -788 -322 -776    0 -356 -129 -229 -189 -511 -594 -209 -579]
 [-170 -532 -149 -480 -960    0 -282    0 -700 -836 -717 -899 -710 -837 -419]
 [-878 -790 -909  -36 -448 -346 -202    0  -76 -245  -37 -615    0 -585 -215]
 [-149 -407 -152 -507 -428  -86 -798 -380    0 -734 -656 -133 -624 -335 -859]
 [ -68    0 -431 -374 -252 -714 -602 -270 -424 -678 -501 -609 -490 -848 -78]]

(defn dumb []
  (let [m (matrix)
        s1 (->> m
                (mapv (fn [coll]
                        (let [mi (apply max coll)]
                          (mapv #(- % mi) coll)))))
        cs (range 0 15)
        s2 (->> cs
                (mapv (fn [i]
                        (mapv (fn [j]
                                (get-in s1 [j i]))
                              cs)))
                (mapv (fn [coll]
                        (let [mi (apply max coll)]
                          (if (zero? mi)
                            coll
                            (mapv #(- % mi) coll))))))
        temp (->> cs
                  (mapv (fn [i]
                          (mapv (fn [j]
                                  (get-in s2 [j i]))
                                cs))))]
    (->> (for [i (range 0 15)]
           [i (for [j (range 0 15)
                    :when (= 0 (get-in temp [j i]))]
                j)]))))

(defn candidate
  [[a b c d e f g]]
  [[0 g] [1 14] [2 7] [3 a] [4 b] [5 11] [6 10] [7 c] [8 d] [9 0] [10 e] [11 9] [12 12] [13 6] [14 f]])


(defn- iter-perm [v]
  (let [len (count v),
        j (loop [i (- len 2)]
            (cond (= i -1) nil
                  (< (v i) (v (inc i))) i
                  :else (recur (dec i))))]
    (when j
      (let [vj (v j),
            l (loop [i (dec len)]
                (if (< vj (v i)) i (recur (dec i))))]
        (loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
          (if (< k l)
            (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
            v))))))


(defn- vec-lex-permutations [v]
  (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))

(defn lex-permutations
  "Fast lexicographic permutation generator for a sequence of numbers"
  [c]
  (lazy-seq
    (let [vec-sorted (vec (sort c))]
      (if (zero? (count vec-sorted))
        (list [])
        (vec-lex-permutations vec-sorted)))))

(defn permutations
  "All the permutations of items, lexicographic by index"
  [items]
  (let [v (vec items)]
    (map #(map v %) (lex-permutations (range (count v))))))

(defn problem-345-a
  []
  (let [m (matrix)]
    (->> (permutations [1 2 3 4 5 8 13])
         (map candidate)
         (map (fn [coll]
                (apply +
                       (map (fn [[i j]]
                              (get-in m [j i])) coll))))
         (apply max))))