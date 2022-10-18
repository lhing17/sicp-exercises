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
  (sqrt-iter 1.0 2),)

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
  (< (Math/abs (- guess (improve guess x))) 0.001))

(comment
  (sqrt-iter 1.0 2)
  (sqrt-iter 1.0 0.00000035)                                ;; 0.0312537295987364
  (sqrt-iter 1.0 2349280230)                                ;; 48469.37414491753
  (* 48469.37414491753 48469.37414491753)

  )

;; Exercise 1.8 Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x,
;; then a better approximation is given by the value
;; (x/y^2 + 2y)/3
;; Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In section 1.3.4 we will
;; see how to implement Newton's method in general as an abstraction of these square-root and cube-root procedures.)
(defn good-enough? [guess new-guess]
  (< (Math/abs (/ (- guess new-guess) guess)) 0.001))

(defn improve [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defn cube-root-iter [guess x]
  (let [new-guess (improve guess x)]
    (if (good-enough? guess new-guess)
      new-guess
      (recur new-guess x))))

(defn cube-root [x]
  (cube-root-iter 1.0 x))

(comment
  (cube-root 27),)

;; Exercise 1.9 Each of the following two procedures defines a method for adding two positive integers in terms of the procedures
;; inc, which increments its argument by 1, and dec, which decrements its argument by 1.
;; (defn + [a b]
;;   (if (= a 0)
;;     b
;;     (inc (+ (dec a) b))))

;; (defn + [a b]
;;   (if (= a 0)
;;     b
;;     (+ (dec a) (inc b))))
;; Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5). Are these processes iterative or recursive?

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; Iterative

;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; Recursive

;; Exercise 1.10 The following procedure computes a mathematical function called Ackermann's function.
;; (defn A [x y]
;;   (cond (= y 0) 0
;;         (= x 0) (* 2 y)
;;         (= y 1) 2
;;         :else (A (- x 1) (A x (- y 1)))))
;; What are the values of the following expressions?
;; (A 1 10)
;; (A 2 4)
;; (A 3 3)
;; Consider the following procedures, where A is the procedure defined above:
;; (defn f [n] (A 0 n))
;; (defn g [n] (A 1 n))
;; (defn h [n] (A 2 n))
;; (defn k [n] (* 5 n n))
;; Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n.
;; For example, (k n) computes 5n^2.

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 4))
;; (A 1 16)
;; 65536

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 4)
;; 65536

(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1) (A x (- y 1)))))
(defn f [n] (A 0 n))
(defn g [n] (A 1 n))
(defn h [n] (A 2 n))
(comment
  (A 1 10)
  (A 2 4)
  (A 3 3)
  (f 10)
  (g 10)
  (h 4)
  (k 10),)
;; f computes 2n, g computes 2^n, h computes 2^(2^n)

;; Exercise 1.11 A function f is defined by the rule that f(n) = n if n < 3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3.
;; Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.
(defn f [n]
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(comment
  (f 10))

(defn f-iter [n a b c]
  (cond (= n 0) a
        (= n 1) b
        (= n 2) c
  :else (recur (dec n) b c (+ c (* 2 b) (* 3 a)))))

(defn f [n]
  (f-iter n 0 1 2))

(comment
  (f 10)
  ,)


