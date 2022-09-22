(ns sicp-exercises.chapter1)

;; Exercise 1.1 Evaluate expressions
(comment
  10
  (+ 5 3 4)
  (- 9 1)
  (/ 6 2)
  (+ (* 2 4) (- 4 6))
  (def a 3)
  (def b (+ a 1))
  (+ a b (* a b))
  (= a b)
  (if (and (> b a) (< b (* a b)))
    b
    a)
  (cond (= a 4) 6
        (= b 4) (+ 6 7 a)
        :else 25)
  (+ 2 (if (> b a) b a))
  (* (cond (> a b) a
           (< a b) b
           :else -1)
     (+ a 1)),)

;; Exercise 1.2 Translate expressions
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3 Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(defn sum-of-squares [a b c]
  (let [max (max a b c)
        min (min a b c)
        mid (+ a b c (- max) (- min))]
    (+ (* max max) (* mid mid))))


(comment
  (sum-of-squares 1 2 3),)

;; Exercise 1.4 Observe that our model of evaluation allows for combinations whose operators are compound expressions.
;; Use this observation to describe the behavior of the following procedure:

(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

(comment
  (a-plus-abs-b 1 -2),)

;; Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using
;; applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(defn p [] (p))

(defn test [x y]
  (if (= x 0)
    0
    y))

;; Then he evaluates the expression
(test 0 (p))

;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
;; What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.
;; (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order:
;; The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)

;; Ben will observe that the interpreter using applicative-order evaluation will enter an infinite loop,
;; while the interpreter using normal-order evaluation will return 0.


;; Exercise 1.6 Alyssa P. Hacker doesn't see why `if` needs to be provided as a special form.
;; "Why can't I just define it as an ordinary procedure in terms of `cond`?" she asks.
;; Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of `if`:
(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

;; Eva demonstrates the program for Alyssa:
(new-if (= 2 3) 0 5)
;; 5
(new-if (= 1 1) 0 5)
;; 0

;; Delighted, Alyssa uses `new-if` to rewrite the square-root program:
(defn good-enough? [guess x]
  (< (Math/abs (- x (* guess guess))) 0.001))

(defn improve [guess x]
  (/ (+ guess (/ x guess)) 2))

(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(comment
  (sqrt-iter 1.0 2)
  ,)

;; What happens when Alyssa attempts to use this to compute square roots? Explain.
;; Alyssa will get an error: Execution error (StackOverflowError) at sicp_exercises.chapter1/sqrt-iter (chapter1.clj:100).

;; Exercise 1.7 The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always performed with limited precision.
;; This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers.
;; An alternative strategy for implementing `good-enough?` is to watch how `guess` changes from one iteration to the next
;; and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of
;; end test. Does this work better for small and large numbers?
(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(defn new-good-enough? [guess x]
  )

(comment
  (sqrt-iter 1.0 2)
  (sqrt-iter 1.0 0.00000035)                                ;; 0.0312537295987364
  (sqrt-iter 1.0 2349280230)               ;; 48469.37414491753
  (* 48469.37414491753 48469.37414491753)


  )
