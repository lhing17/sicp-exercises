(ns sicp_exercises.demo
  (:require [clojure.string :as str]))


(defn fib-recursive [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-recursive (- n 1))
                 (fib-recursive (- n 2)))))

(defn fib-iterative [n]
  (loop [a 1 b 0 k n]
    (if (= k 0)
      b
      (recur (+ a b) a (dec k))
      )))

(comment
  (fib-recursive 10)
  (fib-iterative 10)
  )

(defn first-denomination [kind-of-coins]
  (case kind-of-coins
    1 1
    2 5
    3 10
    4 25
    5 50)
  )

(defn cc [amount kind-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kind-of-coins 0)) 0
        :else (+ (cc amount (dec kind-of-coins))
                 (cc (- amount (first-denomination kind-of-coins)) kind-of-coins))))

(defn count-change [amount]
  (cc amount 5))

(comment
  (count-change 100)
  )

;; print Pascal's triangle
(defn pascal [row col]
  (cond (= col 0) 1
        (= col row) 1
        :else (+ (pascal (dec row) (dec col))
                 (pascal (dec row) col))))

(defn print-row [n]
  (println (->> (iterate inc 0)
                (take (inc n))
                (map #(pascal n %))
                (map str)
                (str/join " ")
                )))

(defn print-pascal [n]
  (loop [k 0]
    (when-not (= k n)
      (print-row k)
      (recur (inc k))
      )))

(comment
  (print-pascal 5))

;; compute exponentiation
(defn square [x] (* x x))
(defn exp [b n]
  (cond (= n 0) 1
        (even? n) (square (exp b (/ n 2)))
        :else (* b (exp b (dec n)))))

(comment (exp 2 30))

(defn square-log [b n]
  )
