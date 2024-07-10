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

;; (A 2 3)
;; (A 1 (A 2 2))
;; (A 1 (A 1 (A 2 1)))
;; (A 1 (A 1 2))
;; (A 1 4)

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
  (h 5)
  (k 10),)
;; f computes 2n, g computes 2^n, h computes 2^(2^(2^...)) (n times)

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
  (f 10),)

;; Exercise 1.12 The following pattern of numbers is called Pascal's triangle.
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; ...
;; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.
;; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

(defn pascal [row col]
  (cond (= col 0) 1
        (= col row) 1
        :else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))

;; Exercise 1.13: Prove that Fib(n) is the closest integer to φ^n / √5, where φ = (1 + √5) / 2 is the golden ratio.
;; Hint: Let ψ = (1 - √5) / 2. Use induction and the definition of the Fibonacci numbers (See Section 1.2.2) to prove that
;; Fib(n) = (φ^n - ψ^n) / √5.

;; Fib(0) = (φ^0 - ψ^0) / √5 = 0
;; Fib(1) = (φ^1 - ψ^1) / √5 = 1
;; Suppose Fib(n - 2) = (φ^(n - 2) - ψ^(n - 2)) / √5 and Fib(n - 1) = (φ^(n - 1) - ψ^(n - 1)) / √5 are true.
;; Then Fib(n) = Fib(n - 2) + Fib(n - 1) = (φ^(n - 2) - ψ^(n - 2)) / √5 + (φ^(n - 1) - ψ^(n - 1)) / √5
;; = (φ^(n - 2) + φ^(n - 1) - ψ^(n - 2) - ψ^(n - 1)) / √5
;;= ( φ^(n - 2) (φ + 1) - ψ^(n - 2) (ψ + 1) / √5

;; Because φ + 1 = φ^2 and ψ + 1 = ψ^2, we can rewrite the above as
;; = (φ^(n - 2) φ^2 - ψ^(n - 2) ψ^2) / √5
;; = (φ^n - ψ^n) / √5

;; Exercise 1.14: Draw the tree illustrating the process generated by the count-change procedure of Section 1.2.2 in
;; making change for 11 cents. What are the orders of growth in space and number of steps used by the process as the
;; amount of be changed increases?

;; (count-change 11)
;; (cc 11 5)
;; (+ (cc 11 4) (cc (- 11 50) 5))
;; (+ (cc 11 4))
;; (+ (cc 11 3) (cc (- 11 25) 4))
;; (+ (cc 11 3))
;; (+ (cc 11 2) (cc (- 11 10) 3))
;; (+ (cc 11 2) (cc 1 3))
;; (+ (cc 11 2) (cc 1 2) (cc (- 1 10) 3))
;; (+ (cc 11 2) (cc 1 2))
;; (+ (cc 11 2) (cc 1 1) (cc (- 1 5) 2))
;; (+ (cc 11 2) 1)
;; (+ (cc 11 1) (cc (- 11 5) 2) 1)
;; (+ (cc 11 1) (cc 6 2) 1)
;; (+ (cc 11 1) (cc 6 1) (cc 1 2) 1)
;; (+ (cc 11 1) (cc 6 1) 2)
;; (+ (cc 11 1) (cc 6 0) (cc (- 6 5) 1) 2)
;; (+ (cc 11 1) 3)
;; (+ (cc 11 0) (cc (- 11 1) 1) 3)
;; 3


;; 1.15: The sine of an angle (specified in radians) can be computed by making use of the approximation sin x ≈ x
;; if x is sufficiently small, and the trigonometric identity
;; sin x = 3 sin (x / 3) − 4 sin^3 (x / 3)
;; to reduce the size of the argument of sin.
;; (For purposes of this exercise an angle is considered “sufficiently small” if its magnitude is not greater than 0.1 radians.) These ideas are
;; incorporated in the following procedures:
(defn cube [x] (* x x x))
(defn p [x]
  (println x)
  (- (* 3 x)
     (* 4 (cube x))))
(defn sin [x]
  (if (<= x 0.1)
    x
    (p (sin (/ x 3.0)))))

(comment
  (sin (/ Math/PI 6))
  (sin 12.15)
  )
;; a. How many times is the procedure p applied when (sin 12.15) is evaluated?
;; 5 times
;; b. What is the order of growth in space and number of steps (as a function of a) used by the process generated by the sine procedure when (sin a) is evaluated?
;; O(log a) in space and O(log a) in number of steps, because the argument is divided by 3 each time.

;; Exercise 1.16: Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.
;; (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the exponent n and the base b, an additional state variable a,
;; and define the state transformation in such a way that the product a b^n is unchanged from state to state. At the beginning of the process a is taken to be 1,
;; and the answer is given by the value of a at the end of the process.
;; In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)
(defn square [x] (* x x))
(defn expt [b n]
  (loop [a 1 b b n n]
    (cond (zero? n) a
          (even? n) (recur a (square b) (/ n 2))
          :else (recur (* a b) b (dec n))
          )))

(comment
  (expt 4 8),)

;; Exercise 1.17: The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication.
;; In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication procedure
;; (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:
;; (defn * [a b]
;;   (if (= b 0)
;;     0
;;     (+ a (* a (- b 1)))))
;; This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition, operations double, which doubles an integer,
;; and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.
(defn my-double [x] (* x 2))
(defn halve [x] (/ x 2))

(defn my-* [a b]
  (loop [a a b b c 0]
    (cond (zero? b) c
          (even? b) (recur (my-double a) (halve b) c)
          :else (recur a (dec b) (+ a c)))))
(comment
  (my-* 4 8),)

;; Exercise 1.18: Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.
(defn my-* [a b]
  (loop [a a b b c 0]
    (cond (zero? b) c
          (even? b) (recur (my-double a) (halve b) c)
          :else (recur a (dec b) (+ a c)))))

;; Exercise 1.19: There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps. Recall the transformation of the state variables a and b in the fib-iter process of section 1.2.2:
;; a <- a + b and b <- a. Call this transformation T, and observe that applying T over and over again n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n).
;; In other words, the Fibonacci numbers are produced by applying T^n, the nth power of the transformation T, starting with the pair (1, 0).
;; Now consider T to be the special case of p = 0 and q = 1 in a family of transformations T(p, q), where T(p, q) transforms the pair (a, b) according to a <- bq + aq + ap and b <- bp + aq.
;; Show that if we apply such a transformation T(p, q) twice, the effect is the same as using a single transformation T(p', q') of the same form, and compute p' and q' in terms of p and q.
;; This gives us an explicit way to square these transformations, and thus we can compute T^n using successive squaring, as in the fast-expt procedure.
;; Put this all together to complete the following procedure, which runs in a logarithmic number of steps:

(defn fib-iter [a b p q count]
  (cond (zero? count) b
        (even? count) (recur a
                             b
                             (+ (* p p) (* q q))
                             (+ (* 2 p q) (* q q))
                             (/ count 2))
        :else (recur (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (dec count))))

;; Exercise 1.20: The process that a procedure generates is of course dependent on the rules used by the interpreter. As an example, consider the iterative gcd procedure given above.
;; Suppose we were to interpret this procedure using normal-order evaluation, as discussed in section 1.1.5. (The normal-order-evaluation rule for if is described in exercise 1.5.)
;; Using the substitution method (for normal order), illustrate the process generated in evaluating (gcd 206 40) and indicate the remainder operations that are actually performed.
;; How many remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))
;; (gcd 206 40) = (gcd 40 (remainder 206 40)) = (gcd 40 6) = (gcd 6 (remainder 40 6)) = (gcd 6 4) = (gcd 4 (remainder 6 4)) = (gcd 4 2) = (gcd 2 (remainder 4 2)) = (gcd 2 0) = 2

;; Exercise 1.21: Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers: 199, 1999, 19999.
(defn divides? [a b] (= (rem b a) 0))
(defn find-divisor [n test-divisor]
  (cond (>= (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (recur n (+ test-divisor 1))))
(defn smallest-divisor [n] (find-divisor n 2))

(comment
  (smallest-divisor 199)
  (smallest-divisor 1999)
  (smallest-divisor 19999),)

;; Exercise 1.22: Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds).
;; The following timed-prime-test procedure, when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.
(defn exp-mod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (exp-mod base (/ exp 2) m)) m)
        :else (rem (* base (exp-mod base (- exp 1) m)) m)))

(defn try-it [a n]
  (= (exp-mod a n n) a))
(defn fermat-test [n]
  (try-it (+ 1 (rand-int (- n 1))) n))

(defn fast-prime? [n times]
  (cond (zero? times) true
        (fermat-test n) (recur n (dec times))
        :else false))

(defn prime? [n]
  (fast-prime? n 100000))


(defn report-prime [elapsed-time]
  (print " *** ")
  (print elapsed-time))
(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime (- (System/currentTimeMillis) start-time))))
(defn timed-prime-test [n]
  (println)
  (print n)
  (start-prime-test n (System/currentTimeMillis)))

(comment
  (timed-prime-test 1000091)
  )
;; Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range.
;; Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000.
;; Note the time needed to test each prime. Since the testing algorithm has order of growth of Θ(√n), you should expect that testing for primes around 10,000 should take about √10 times as long as testing for primes around 1000.
;; Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the √n prediction?
;; Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?
(defn search-for-primes [n]
  (let [start-time (System/currentTimeMillis)]
    (->> (iterate inc (inc n))
         (filter odd?)
         (filter prime?)
         (take 3)
         (print)
         )
    (println "Elapsed time:" (- (System/currentTimeMillis) start-time))
    ))

(comment
  (search-for-primes 1000)
  (search-for-primes 10000)
  (search-for-primes 100000)
  (search-for-primes 1000000),)

;; Exercise 1.23: The smallest-divisor procedure shown at the start of this section does lots of needless testing:
;; After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers.
;; This suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement this change,
;; define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2.
;; Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1).
;; With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12 primes found in exercise 1.22.
;; Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed?
;; If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?



;; Exercise 1.29: Simpson's Rule is a more accurate method of numerical integration than the method illustrated above.
;; Using Simpson's Rule, the integral of a function f between a and b is approximated as:
;; (h / 3) * (y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 4yn-1 + yn)
;; where h = (b - a) / n, for some even integer n, and yk = f(a + kh).
;; (Increasing n increases the accuracy of the approximation.)
;; Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson's Rule.
;; Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.

(defn simpson [f a b n]
  (let [h (/ (- b a) n)
        y (fn [k] (f (+ a (* k h))))]
    (* (/ h 3)
       (+ (f a)
          (f b)
          (->> (range 1 n)
               (filter odd?)
               (map (fn [i] (* 2 (y i))))
               (apply +)
               )
          (->> (range 1 n)
               (filter even?)
               (map (fn [i] (* 4 (y i))))
               (apply +)
               )))))
(defn cube [x] (* x x x))

(comment
  (simpson cube 0 1 10000)
  (simpson cube 0 1 100)
  )

;; Exercise 1.30: The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively.
;; Show how to do this by filling in the missing expressions in the following definition:
(defn sum [term a next b]
  (loop [a a result 0]
    (if (> a b)
      result
      (recur (next a) (+ result (term a))))))

(comment
  (sum square 1 inc 10)
  )

;; Exercise 1.31: a. The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.
;; Write an analogous procedure called product that returns the product of the values of a function at points over a given range.
;; Show how to define factorial in terms of product. Also use product to compute approximations to π using the formula:
;; π / 4 = 2 * 4 * 4 * 6 * 6 * 8 * ... / 3 * 3 * 5 * 5 * 7 * 7 * ...
;; b. If your product procedure generates a recursive process, write one that generates an iterative process.
(defn product [term a next b]
  (loop [a a result 1]
    (if (> a b)
      result
      (recur (next a) (* result (term a))))))

(defn factorial [n]
  (product identity 1 inc n))

(defn pi [n]
  (* 4.0 (product (fn [x]
                    (if (even? x)
                      (/ (+ x 2) (+ x 1))
                      (/ (+ x 1) (+ x 2))))
                  1 inc n)))


(comment
  (factorial 5)
  (product (fn [x] (* x x)) 1 inc 10)
  (pi 10000)
  )

(defn product-rec [term a next b]
  (if (> a b)
    1
    (* (term a) (product-rec term (next a) next b))))

;; Exercise 1.32: a. Show that sum and product (exercise 1.31) are both special cases of a still more general abstraction called accumulate that combines a collection of terms, using some general accumulation function:
;; (accumulate combiner null-value term a next b)
;; accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner
;; procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding
;; terms and a null-value that specifies what base value to use when the terms run out.
;; Write accumulate and show how sum and product can both be defined as simple calls to accumulate.
;; b. If your accumulate procedure generates a recursive process, write one that generates an iterative process.
(defn accumulate [combiner null-value term a next b]
  (loop [a a result null-value]
    (if (> a b)
      result
      (recur (next a) (combiner result (term a)))
      )))

(defn sum [term a next b]
  (accumulate + 0 term a next b))

(defn product [term a next b]
  (accumulate * 1 term a next b))

(comment
  (sum square 1 inc 10)
  (product (fn [x] (* x x)) 1 inc 10)
  )

(defn accumulate-rec [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))


;; Exercise 1.33: You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined.
;; That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter.
;; Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:
;; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)
;; b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i, n) = 1).
(defn filtered-accumulate [combiner null-value term a next b pred]
  (loop [a a result null-value]
    (if (> a b)
      result
      (recur (next a) (if (pred a) (combiner result (term a)) result)))))

(defn prime? [n]
  (not-any? #(zero? (rem n %)) (range 2 n)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

(defn relatively-prime? [n]
  (fn [x]
    (= 1 (gcd x n))))

(comment
  (filtered-accumulate + 0 square 2 inc 10 prime?)          ;; 4 + 9 + 25 + 49 = 87
  (filtered-accumulate * 1 identity 1 inc 10 (relatively-prime? 10)) ;; 1 * 3 * 7 * 9 = 189
  (accumulate * 1 identity 1 inc 10)
  )

;; Exercise 1.34: Suppose we define the procedure:
(defn f [g] (g 2))
;; Then we have:
(comment
  (f square)
  (f f)
  )
;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

;; (f f) = (f 2) = (2 2) = Execution error (ClassCastException) java.lang.Long cannot be cast to clojure.lang.IFn

;; Exercise 1.35: Show that the golden ratio φ (1 + √5) / 2 is a fixed point of the transformation x -> 1 + 1 / x, and use this fact to compute φ by means of the fixed-point procedure.
(defn fixed-point [f first-guess]
  (let [close-enough? (fn [v1 v2] (< (Math/abs (- v1 v2)) 0.00001))
        try-guess (fn [guess]
                    (let [next-guess (f guess)]
                      (if (close-enough? guess next-guess)
                        next-guess
                        (recur next-guess))))]
    (try-guess first-guess)))

(defn golden-ratio [x] (+ 1 (/ 1 x)))
(comment
  (fixed-point golden-ratio 1.0)
  )

;; Exercise 1.36: Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and display primitives shown in exercise 1.22.
;; Then find a solution to x^x = 1000 by finding a fixed point of x -> log(1000) / log(x). (Use the primitive Scheme log procedure, which computes natural logarithms.)
;; Compare the number of steps this takes with and without average damping. (Note that you cannot start fixed-point with a guess of 1 for this problem.)
(defn fixed-point [f first-guess]
  (let [close-enough? (fn [v1 v2]
                        (< (/ (Math/abs (- v1 v2)) v1) 0.00001
                        ))
        try-guess (fn [guess]
                    (let [next-guess (f guess)]
                      ;(println next-guess)
                      (if (close-enough? guess next-guess)
                        next-guess
                        (recur next-guess))))]
    (try-guess first-guess)))

(defn average-damp [f]
  (fn [x] (/ (+ x (f x)) 2)))

(comment
  (fixed-point (average-damp (fn [x] (/ (Math/log 1000) (Math/log x)))) 2.0)
  )

;; Exercise 1.37: a. An infinite continued fraction is an expression of the form:
;; f = N1 / (D1 + N2 / (D2 + N3 / (D3 + ...)))
;; As an example, one can show that the infinite continued fraction expansion with the Ni and Di all equal to 1 produces 1 / φ, where φ is the golden ratio (described in section 1.2.2).
;; One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called k-term finite continued fraction -- has the form:
;; f = N1 / D1 + N2 / (D2 + N3 / (... + Nk / Dk))
;; Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction.
;; Check your procedure by approximating 1 / φ using
;; (cont-frac (fn [i] 1.0) (fn [i] 1.0) k)
;; for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?
;; b. If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
(defn cont-frac [n d k]
  (loop [i k result 0]
    (if (zero? i)
      result
      (recur (dec i) (/ (n i) (+ (d i) result))))))

(comment
  (cont-frac (fn [i] 1.0) (fn [i] 1.0) 10)
  (cont-frac (fn [i] 1.0) (fn [i] 1.0) 12)
  (cont-frac (fn [i] 1.0) (fn [i] 1.0) 1000)
  (cont-frac (fn [i] 1.0) (fn [i] 1.0) 10000)
  )

;; Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis,
;; which included a continued fraction expansion for e - 2, where e is the base of the natural logarithms.
;; In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....
;; Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler's expansion.
(defn e-approx [k]
  (+ 2 (cont-frac (fn [i] 1.0) (fn [i] (if (= 2 (rem i 3))
                                         (* 2 (inc (/ (- i 2) 3)))
                                         1.0)) k)))

(comment
  (e-approx 10)
  (e-approx 100)
  (e-approx 1000)
  )

;; Exercise 1.39: A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert:
;; tan x = x / (1 - x^2 / (3 - x^2 / (5 - x^2 / (7 - ...))))
;; where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert's formula.
;; k specifies the number of terms to compute, as in exercise 1.37.
(defn tan-cf [x k]
  (cont-frac (fn [i] (if (= 1 i)
                       x
                       (- (* x x))))
             (fn [i] (- (* 2 i) 1))
             k))

(comment
  (tan-cf (/ Math/PI 6) 10)
  (tan-cf (/ Math/PI 6) 100)
  (tan-cf (/ Math/PI 6) 1000)
  )

;; Exercise 1.40: Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form
;; (newtons-method (cubic a b c) 1)
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
(def dx 0.00001)
(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))

(comment
  ((deriv (fn [x] (* x x x))) 2)
  )

(defn newtons-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newtons-transform g) guess))

(defn sqrt [x]
  (newtons-method (fn [y]
                    (- (* y y) x))
                  1.0))
(comment
  (sqrt 2)
  )

(defn cubic [a b c]
  (fn [x] (+ (cube x) (* a (square x)) (* b x) c)))

(comment
  (newtons-method (cubic -3 3 -1) 2)
  )

;; Exercise 1.41: Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice.
;; For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2.
;; What value is returned by (((double (double double)) inc) 5)?
(defn double [f]
  (fn [x] (f (f x))))

(comment
  (((double (double double)) inc) 5)
  (((double double) inc) 5)
  ((double (fn [x] (inc (inc x)))) 5)
  )

;; Exercise 1.42: Let f and g be two one-argument functions. The composition f after g is defined to be the function x -> f(g(x)).
;; Define a procedure compose that implements composition. For example, if inc is a procedure that adds 1 to its argument, ((compose square inc) 6) should return 49.
(defn compose [f g]
  (fn [x] (f (g x))))

(comment
  ((compose square inc) 6)
  )

;; Exercise 1.43: If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f, which is defined to be the function whose value at x is f(f(...(f(x))...)). For example, if f is the function x -> x + 1, then the nth repeated application of f is the function x -> x + n.
;; If f is the function x -> x * x, then the nth repeated application of f is the function that raises x to the 2^n-th power.
;; Write a procedure that takes as inputs a procedure that computes f and a positive integer n and returns the procedure that computes the nth repeated application of f.
;; Your procedure should be able to be used as follows:
;; ((repeated square 2) 5)
(defn repeated [f n]
  (loop [n (dec n) result f]
    (if (zero? n)
      result
      (recur (dec n) (compose f result)))))

(comment
  ((repeated square 2) 5)
  ((repeated inc 3) 5)
  )


;; Exercise 1.44: The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is a small number, then the smoothed version of f is the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx).
;; Write a procedure smooth that takes as input a function that computes f and returns a function that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtain the n-fold smoothed function.
;; Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exercise 1.43.
(defn smooth [f]
  (fn [x] (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(defn n-fold-smoothed [f n]
  ((repeated smooth n) f))

(comment
  ((smooth square) 2)
  ((n-fold-smoothed square 2) 2)
  ((n-fold-smoothed square 3) 2)
  )

;; Exercise 1.45: We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of the average-damp transformation does not converge,
;; and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damp transformation.
;; Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for x -> x^4 converge.
;; On the other hand, if we average damp twice (i.e., use the average-damp of the average-damp of x -> x^4), the fixed-point search does converge.
;; Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of x -> x^n-1.
;; Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of exercise 1.43.
;; Assume that any arithmetic operations you need are available as primitives.
(defn nth-root [n]
  (fn [x] (fixed-point (repeated (average-damp (fn [y] (/ x (Math/pow y (- n 1))))) 2) 1)))

(comment
  ((nth-root 2) 2)
  ((nth-root 3) 2)
  ((nth-root 4) 2)
  ((nth-root 5) 2)
  ((nth-root 6) 2)
  ((nth-root 7) 2)
  ((nth-root 8) 2)
  ((nth-root 9) 2)
  ((nth-root 10) 2)
  ((nth-root 11) 2)
  ((nth-root 17) 2)

  (fixed-point (average-damp #(/ 2 %)) 1.0)
  )

;; Exercise 1.46: Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement.
;; Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess.
;; Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess.
;; Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough.
;; Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of iterative-improve.
(defn iterative-improve [good-enough? improve]
  (fn [guess]
    (if (good-enough? guess (improve guess))
      guess
      (recur (improve guess)))))

(defn average [x y]
  (/ (+ x y) 2))

(defn sqrt [x]
  ((iterative-improve (fn [guess next-guess] (< (java.lang.Math/abs (- guess next-guess)) 0.00001))
                     (fn [guess] (average guess (/ x guess))))
                     1.0))

(comment
  (sqrt 2)
  )

(defn fixed-point [f first-guess]
  ((iterative-improve (fn [guess next-guess] (< (java.lang.Math/abs (- guess next-guess)) 0.00001))
                     (fn [guess] (f guess)))
                     first-guess))

(comment
  (fixed-point (average-damp #(/ 2 % % %)) 1.0)
  )
