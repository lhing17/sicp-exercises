(ns sicp-exercises.chapter2)

;; Exercise 2.1: Define a better version of make-rat that handles both positive and negative arguments.
;; make-rat should normalize the sign so that if the rational number is positive, both the numerator and
;; the denominator are positive, and if the rational number is negative, only the numerator is negative.
(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))
(defn make-rat
  [n d]
  (let [sign (if (or (and (neg? n) (pos? d)) (and (pos? n) (neg? d))) -1 1)
        n (Math/abs n)
        d (Math/abs d)
        g (gcd n d)]
    (list (* sign (/ n g)) (/ d g))))

(make-rat 8 -6)

;; Exercise 2.2: Consider the problem of representing line segments in a plane. Each segment is represented
;; as a pair of points: a starting point and an ending point. Define a constructor make-segment and selectors
;; start-segment and end-segment that define the representation of segments in terms of points. Furthermore,
;; a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify
;; a constructor make-point and selectors x-point and y-point that define this representation. Finally, using
;; your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument
;; and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints).
;; To try your procedures, you'll need a way to print points:
(defn make-point [x y]
  (list x y))

(defn x-point [p]
  (first p))

(defn y-point [p]
  (last p))

(defn print-point [p]
  (->> ["(" (x-point p) ", " (y-point p) ")"]
       (apply str)
       (println)))

(defn make-segment [start end]
  (list start end))

(defn start-segment [s]
  (first s))

(defn end-segment [s]
  (last s))

(defn midpoint-segment [s]
  (let [start (start-segment s)
        end (end-segment s)]
    (make-point (/ (+ (x-point start) (x-point end)) 2)
                (/ (+ (y-point start) (y-point end)) 2))))

(comment
  (def p1 (make-point 1 2))
  (def p2 (make-point 3 4))
  (def s (make-segment p1 p2))
  (print-point (midpoint-segment s))
  )

;; Exercise 2.3: Implement a representation for rectangles in a plane. (Hint: You may want to make use of your
;; make-point and make-segment procedures from Exercise 2.2.) In terms of your constructors and selectors, create
;; procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation
;; for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and
;; area procedures will work using either representation?
(defn make-rectangle [min-x min-y max-x max-y]
  (list (make-point min-x min-y) (make-point max-x max-y)))

(defn width-rectangle [r]
  (- (x-point (last r)) (x-point (first r))))

(defn height-rectangle [r]
  (- (y-point (last r)) (y-point (first r))))

(defn make-rectangle2 [up-seg, left-seg]
  (list up-seg left-seg))

(defn width-rectangle2 [r]
  (- (x-point (end-segment (first r))) (x-point (start-segment (first r)))))

(defn height-rectangle2 [r]
  (- (y-point (end-segment (last r))) (y-point (start-segment (last r)))))




(defn perimeter-rectangle [r]
  (* 2 (+ (width-rectangle2 r) (height-rectangle2 r))))

(defn area-rectangle [r]
  (* (width-rectangle2 r) (height-rectangle2 r)))

(comment
  (def r (make-rectangle 1 2 3 4))
  (println (perimeter-rectangle r))
  (println (area-rectangle r))

  (def r2 (make-rectangle2 (make-segment (make-point 1 4) (make-point 3 4))
                           (make-segment (make-point 1 2) (make-point 1 4))))
  (println (perimeter-rectangle r2))
  (println (area-rectangle r2))
  )


;; Exercise 2.4: Here is an alternative procedural representation of pairs. For this representation, verify that
;; (car (cons x y)) yields x for any objects x and y.
(defn cons [a b]
  (fn [m] (m a b)))
(defn car [z]
  (z (fn [p q] p)))
;; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the substitution
;; model of Section 1.1.5.)
(defn cdr [z]
  (z (fn [p q] q)))

(comment
  (println (car (cons 1 2)))
  (println (cdr (cons 1 2)))
  )

;; Exercise 2.5: Show that we can represent pairs of nonnegative integers using only numbers and arithmetic
;; operations if we represent the pair a and b as the integer that is the product 2^a * 3^b. Give the corresponding
;; definitions of the procedures cons, car, and cdr.
(defn cons2 [a b]
  (* (Math/pow 2 a) (Math/pow 3 b)))

(defn car2 [z]
  (loop [result 0 z z]
    (if (zero? (rem z 2))
      (recur (inc result) (quot z 2))
      result)))

(defn cdr2 [z]
  (loop [result 0 z z]
    (if (zero? (rem z 3))
      (recur (inc result) (quot z 3))
      result)))

(comment
  (println (car2 (cons2 5 8)))
  (println (cdr2 (cons2 5 8)))
  )

;; Exercise 2.6: In case representing pairs as procedures wasn't mind-boogling enough, consider that, in a language
;; that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are
;; concerned) by implementing 0 and the operation of adding 1 as:
(defn zero []
  (fn [f]
    (fn [x] x)))

(defn add1 [n]
  (fn [f]
    (fn [x] (f ((n f) x)))))
;; This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented
;; the lambda calculus.
;; Define one and two directly (not in terms of zero and add1). (Hint: Use substitution to evaluate (add1 zero).)
;; Give a direct definition of the addition procedure + (not in terms of repeated application of add1).
(defn one []
  (fn [f]
    (fn [x] (f x))))

(defn two []
  (fn [f]
    (fn [x] (f (f x)))))

(defn add [a b]
  (fn [f]
    (fn [x] ((a f) ((b f) x)))))

;; Exercise 2.7: Alyssa's program is incomplete because she has not specified the implementation of the interval
;; abstraction. Here is a definition of the interval constructor:
(defn make-interval [a b]
  (list a b))
;; Define selectors upper-bound and lower-bound to complete the implementation.
(defn lower-bound [i]
  (first i))
(defn upper-bound [i]
  (last i))

;; Exercise 2.8: Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed.
;; Define a corresponding subtraction procedure, called sub-interval.
(defn sub-interval [i1 i2]
  (make-interval (- (lower-bound i1) (upper-bound i2))
                 (- (upper-bound i1) (lower-bound i2))))

;; Exercise 2.9: The width of an interval is half of the difference between its upper and lower bounds. The width is
;; a measure of the uncertainty of the number specified by the interval. For some arithmetic operations, the width of
;; the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others
;; the width of the combination is not a function of the widths of the argument intervals. Show that the width of the
;; sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted).
;; Give examples to show that this is not true for multiplication or division.
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))
;; width of the sum of two intervals is the sum of the widths of the intervals
;; width of the difference of two intervals is the difference of the widths of the intervals


;; Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is
;; not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition
;; and to signal an error if it occurs.
(defn mul-interval [i1 i2]
  (let [p1 (* (lower-bound i1) (lower-bound i2))
        p2 (* (lower-bound i1) (upper-bound i2))
        p3 (* (upper-bound i1) (lower-bound i2))
        p4 (* (upper-bound i1) (upper-bound i2))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(defn div-interval [i1 i2]
  (if (or (<= (lower-bound i2) 0) (>= (upper-bound i2) 0))
    (throw (IllegalArgumentException. "Interval spans zero"))
    (mul-interval i1 (make-interval (/ 1.0 (upper-bound i2)) (/ 1.0 (lower-bound i2))))))

;; Exercise 2.11: In passing, Ben also cryptically comments: "By testing the signs of the endpoints of the intervals,
;; it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications."
;; Rewrite this procedure using Ben's suggestion.
(defn mul-interval2 [i1 i2]
  (let [a (lower-bound i1)
        b (upper-bound i1)
        c (lower-bound i2)
        d (upper-bound i2)]
    (cond
      (and (pos? a) (pos? c)) (make-interval (* a c) (* b d))
      (and (pos? a) (neg? d)) (make-interval (* b c) (* a d))
      (and (neg? b) (pos? c)) (make-interval (* a d) (* b c))
      (and (neg? b) (neg? d)) (make-interval (* b d) (* a c))
      (and (pos? a) (neg? c) (pos? d)) (make-interval (* b c) (* b d))
      (and (neg? b) (neg? c) (pos? d)) (make-interval (* a d) (* a c))
      (and (neg? a) (pos? b) (pos? c)) (make-interval (* a d) (* b d))
      (and (neg? a) (pos? b) (neg? d)) (make-interval (* b c) (* a c))
      :else (make-interval (min (* a c) (* a d) (* b c) (* b d))
                           (max (* a c) (* a d) (* b c) (* b d))))))

;; Exercise 2.12: After debugging her program, Alyssa shows it to a potential user, who complains that her program
;; solves the wrong problem. He wants a program that can deal with numbers represented as a center value and an
;; additive tolerance; for example, he wants to work with intervals such as 3.5 ± 0.15 rather than 3.35 to 3.65.
;; Alyssa returns to her desk and fixes this problem by supplying an alternate constructor and alternate selectors:
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn width2 [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))
;; Unfortunately, most of Alyssa's users are engineers. Real engineering situations usually involve measurements
;; with only a small uncertainty, measured as the ratio of the width of the interval to the midpoint of the interval.
;; Engineers usually specify percentage tolerances on the parameters of devices, as in the resistor specifications
;; given earlier. Define a constructor make-center-percent that takes a center and a percentage tolerance and produces
;; the desired interval. You must also define a selector percent that produces the percentage tolerance for a given
;; interval. The center selector is the same as the one shown above.
(defn make-center-percent [c p]
  (let [w (* c p)]
    (make-center-width c w)))
(defn percent [i]
  (/ (* 100 (width2 i)) (center i)))

;; Exercise 2.17: Define a procedure last-pair that returns the list that contains only the last element of a given
;; (nonempty) list:

(defn last-pair [l]
  (if (empty? (rest l))
    (first l)
    (recur (rest l))))

(comment
  (println (last-pair '(1 2 3 4 5)))
  )

;; Exercise 2.18: Define a procedure reverse that takes a list as argument and returns a list of the same elements in
;; reverse order:
(defn reverse [l]
  (loop [result '() l l]
    (if (empty? l)
      result
      (recur (clojure.core/cons (first l) result) (rest l)))))

(comment
  (println (reverse '(1 2 3 4 5)))
  )

;; Exercise 2.20: The procedures +, *, and list take arbitrary numbers of arguments. One way to define such procedures
;; is to use define with dotted-tail notation. In a procedure definition, a parameter list that has a dot before the
;; last parameter indicates that, when the procedure is called, the initial parameters (if any) will have as values the
;; initial arguments, as usual, but the final parameter's value will be a list of any remaining arguments. For example,
;; given the definition
;; (define (f x y . z) <body>)
;; the procedure f can be called with two or more arguments. If we evaluate
;; (f 1 2 3 4 5 6)
;; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Use this notation to write a
;; procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same
;; even-odd parity as the first argument. For example,
;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)
;; (same-parity 2 3 4 5 6 7)
;; (2 4 6)

(defn same-parity [x & xs]
  (filter #(zero? (mod (+ x %) 2)) (clojure.core/cons x xs)))

(comment
  (println (same-parity 1 2 3 4 5 6 7))
  (println (same-parity 2 3 4 5 6 7))
  )

;; Exercise 2.21: The procedure square-list takes a list of numbers as argument and returns a list of the squares of
;; the numbers in the list. Here are two different definitions of square-list. Complete both of them by filling in the
;; missing expressions:
(defn square-list [items]
  (map #(* % %) items))

(defn square-list2 [items]
  (if (empty? items)
    '()
    (clojure.core/cons (* (first items) (first items)) (square-list2 (rest items)))))

(comment
  (println (square-list '(1 2 3 4 5)))
  (println (square-list2 '(1 2 3 4 5)))
  )

;; Exercise 2.22: Louis Reasoner tries to rewrite the first square-list procedure of Exercise 2.21 so that it evolves
;; an iterative process:
(defn square-list3 [items]
  (loop [result [] items items]
    (if (empty? items)
      result
      (recur (conj result (* (first items) (first items))) (rest items)))))
;; Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?
;; Louis then tries to fix his bug by interchanging the arguments to cons:
(defn square-list4 [items]
  (loop [result '() items items]
    (if (empty? items)
      result
      (recur (clojure.core/cons result (* (first items) (first items))) (rest items)))))
;; This doesn't work either. Explain.

(comment
  (println (square-list3 '(1 2 3 4 5)))
  (println (square-list4 '(1 2 3 4 5)))
  )

;; Exercise 2.23: The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements.
;; However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in
;; turn, from left to right. The values returned by applying the procedure to the elements are not used at all. For
;; example,
;; (for-each (lambda (x) (newline) (display x))
;; '(57 321 88))
;; will display
;; 57
;; 321
;; 88
;; at the terminal. The value returned by the call to for-each (not illustrated above) can be something arbitrary,
;; such as true. Give an implementation of for-each.
(defn for-each [f items]
  (if (empty? items)
    true
    (do
      (f (first items))
      (recur f (rest items)))))

(comment
  (for-each println '(57 321 88))
  )

;; Exercise 2.24: Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed by the
;; interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in Figure 2.6).
;; (1 (2 (3 4)))
;; 1 -> 2 -> 3 -> 4


;; Exercise 2.25: Give combinations of cars and cdrs that will pick 7 from each of the following lists:
;; (1 3 (5 7) 9)
;; ((7))
;; (1 (2 (3 (4 (5 (6 7))))))
(def car first)
(def cdr rest)
(def cadr (comp car cdr))

(comment
  (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
  (car (car '((7))))
  (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))

  )

;; Exercise 2.26: Suppose we define x and y to be two lists:
;; (define x (list 1 2 3))
;; (define y (list 4 5 6))
;; What result is printed by the interpreter in response to evaluating each of the following expressions:
;; (append x y)
;; (cons x y)
;; (list x y)
(def x '(1 2 3))
(def y '(4 5 6))

(comment
  (println (concat x y))
  (println (clojure.core/cons x y))
  (println (list x y))
  )

;; Exercise 2.27: Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse procedure that takes a list
;; as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well.
(defn deep-reverse [l]
  (loop [result '() l l]
    (if (empty? l)
      result
      (recur (clojure.core/cons (if (list? (first l)) (deep-reverse (first l)) (first l)) result) (rest l)))))

(comment
  (println (deep-reverse '(1 2 (3 4 (5 6)) 7 8)))
  (reverse '(1 2 (3 4 (5 6)) 7 8))
  )

;; Exercise 2.28: Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list
;; whose elements are all the leaves of the tree arranged in left-to-right order. For example,
;; (fringe (list 1 (list 2 (list 3 4) 5) (list 6 7))
;; (1 2 3 4 5 6 7)
(defn fringe [t]
  (if (empty? t)
    []
    (if (list? (first t))
      (concat (fringe (first t)) (fringe (rest t)))
      (clojure.core/cons (first t) (fringe (rest t))))))

(defn fringe-by-reduce [t]
  (reduce (fn [acc x]
            (if (list? x)
              (vec (concat acc (fringe-by-reduce x)))
              (conj acc x)))
          []
          t))


(comment
  (println (fringe '(1 (2 (3 4) 5) (6 7))))
  (println (fringe-by-reduce '(1 (2 (3 4) 5) (6 7))))
  (fringe-by-reduce '(1 (2)))
  )

;; Exercise 2.29: A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of
;; a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile
;; using compound data by constructing it from two branches (for example, using list):
;; (define (make-mobile left right) (list left right))
;; A branch is constructed from a length (which must be a number) together with a structure, which may be either a
;; number (representing a simple weight) or another mobile:
;; (define (make-branch length structure) (list length structure))
;; a. Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and
;; branch-length and branch-structure, which return the components of a branch.
;; b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.
;; c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its
;; top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal
;; to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced.
;; Design a predicate that tests whether a binary mobile is balanced.
;; d. Suppose we change the representation of mobiles so that the constructors are
;; (define (make-mobile left right) (cons left right))
;; (define (make-branch length structure) (cons length structure))
;; How much do you need to change your programs to convert to the new representation?
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [m]
  (first m))

(defn right-branch [m]
  (last m))

(defn branch-length [b]
  (first b))

(defn branch-structure [b]
  (last b))

(defn total-weight [m]
  (let [left (left-branch m)
        right (right-branch m)]
    (+ (if (number? (branch-structure left))
         (branch-structure left)
         (total-weight (branch-structure left)))
       (if (number? (branch-structure right))
         (branch-structure right)
         (total-weight (branch-structure right))))))

(defn balanced? [m]
  (let [left (left-branch m)
        right (right-branch m)
        left-weight (if (number? (branch-structure left))
                      (branch-structure left)
                      (total-weight (branch-structure left))
                      )
        right-weight (if (number? (branch-structure right))
                       (branch-structure right)
                       (total-weight (branch-structure right))
                       )
        left-torque (* (branch-length left) left-weight)
        right-torque (* (branch-length right) right-weight)]
    (and (= left-torque right-torque)
         (if (number? (branch-structure left))
           true
           (balanced? (branch-structure left)))
         (if (number? (branch-structure right))
           true
           (balanced? (branch-structure right))))))

;; Exercise 2.30: Define a function square-tree that squares the leaves of a tree. That is, square-tree should return a
;; tree of the same shape as the original tree, but with all leaves squared. For example, (square-tree (list 1 (list 2
;; (list 3 4) 5) (list 6 7))) should return (list 1 (list 4 (list 9 16) 25) (list 36 49)).
(defn square-tree [tree]
  (map (fn [subtree]
         (if (list? subtree)
           (square-tree subtree)
           (* subtree subtree)))
       tree))

(defn square-tree2 [tree]
  (cond (and (list? tree) (empty? tree)) '()
        (list? tree) (clojure.core/cons (square-tree2 (first tree)) (square-tree2 (rest tree)))
        :else (* tree tree)))

(comment
  (println (square-tree '(1 (2 (3 4) 5) (6 7))))
  (println (square-tree2 '(1 (2 (3 4) 5) (6 7))))
  )

;; Exercise 2.31: Abstract your answer to exercise 2.30 to produce a function tree-map with the property that square-tree
;; could be defined as
;; (define (square-tree tree) (tree-map square tree))

(defn tree-map [f tree]
  (map (fn [subtree]
         (if (list? subtree)
           (tree-map f subtree)
           (f subtree)))
       tree))

(comment
  (println (tree-map #(* % %) '(1 (2 (3 4) 5) (6 7))))
  )


;; Exercise 2.32: We can represent a set as a list of distinct elements, and we can represent the set of all subsets of
;; the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1)
;; (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set
;; and give a clear explanation of why it works:
(defn subsets [s]
  (if (empty? s)
    '(())
    (let [rest (subsets (rest s))]
      (concat rest (map (fn [x] (clojure.core/cons (first s) x)) rest)))))

(comment
  (println (subsets '(1 2 3)))
  )
;; The procedure works by recursively generating the subsets of the rest of the set and then adding the first element
;; of the set to each of the subsets of the rest of the set.


;; Exercise 2.33: Fill in the missing expressions to complete the following definitions of some basic list-manipulation
;; operations as accumulations:
(defn map2 [f l]
  (reduce (fn [acc x] (conj acc (f x))) [] l))

(defn append2 [l1 l2]
  (reduce clojure.core/cons l2 l1))

(defn length2 [l]
  (reduce (fn [acc x] (inc acc)) 0 l))

(comment
  (map2 #(* % %) '(1 2 3 4 5))
  (append2 '(1 2 3) '(4 5 6))
  (length2 '(1 2 6 4 8))
  )


;; Exercise 2.34: Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We evaluate
;; the polynomial
;; a_n x^n + a_{n-1} x^{n-1} + ... + a_1 x + a_0
;; using a well-known algorithm called Horner's rule, which structures the computation as
;; (... (a_n x + a_{n-1}) x + ... + a_1) x + a_0
;; In other words, we start with a_n, multiply by x, add a_{n-1}, multiply by x, and so on, until we reach a_0. Fill in
;; the following template to produce a procedure that evaluates a polynomial using Horner's rule. Assume that the
;; coefficients of the polynomial are arranged in a sequence, from a_0 through a_n.

(defn horner-eval [x coefficient-sequence]
  (reduce (fn [acc a] (+ acc (* x a))) 0 coefficient-sequence))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence) (accumulate op initial (rest sequence)))))

(defn horner-eval2 [x coefficient-sequence]
  (accumulate (fn [a b] (+ a (* x b))) 0 coefficient-sequence))

(comment
  (println (horner-eval 2 '(1 3 0 5 0 1)))
  (println (horner-eval2 2 '(1 3 0 5 0 1)))

  (accumulate #(+ (* % 2 %2) %2) 1 [1 2 3 4 5])
  )


;; Exercise 2.35: Redefine count-leaves from section 2.2.2 as an accumulation:
(defn count-leaves [x]
  (accumulate (fn [a b] (+ a b)) 0 (map (fn [y] (if (list? y) (count-leaves y) 1)) x)))

;; Exercise 2.36: The procedure accumulate-n is similar to accumulate, except that it takes as its third argument a
;; sequence of sequences, which are all assumed to have the same number of elements. It applies the designated
;; accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences,
;; and so on, and returns a sequence of the results. For instance, if s is a sequence containing four sequences,
;; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be (22 26 30). Fill in the
;; missing expressions in the following definition of accumulate-n:
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    '()
    (clojure.core/cons (accumulate op init (map first seqs))
                       (accumulate-n op init (map rest seqs)))))

(comment
  (println (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  )


;; Exercise 2.37: Suppose we represent vectors v = (v_1 v_2 ... v_n) as sequences of numbers, and matrices m as sequences
;; of vectors (the rows of the matrix). For example, the matrix
;; 1 2 3 4
;; 4 5 6 6
;; 6 7 8 9
;; would be represented as ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations
;; to concisely express the basic matrix and vector operations. These operations (which are described in any book on
;; matrix algebra) are the following:
;; - Vector dot product: (dot-product v w) = v_1 w_1 + ... + v_n w_n
;; - Matrix multiplication: (matrix-*-vector m v) = (v_1 dot-product row_1) ... (v_n dot-product row_n)
;; - Matrix multiplication: (matrix-*-matrix m n) = ((row_1-*-column_1) ... (row_1-*-column_n)) ... ((row_n-*-column_1) ... (row_n-*-column_n))
;; - Transpose: (transpose m) = ((m_1 1 ... m_n 1) ... (m_1 n ... m_n n))
;; We can define the dot product as
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))
;; Fill in the missing expressions in the following procedures for computing the other matrix operations:
(defn matrix-*-vector [m v]
  (map #(dot-product % v) m))

(defn transpose [m]
  (accumulate-n clojure.core/cons '() m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

(comment
  (dot-product '(1 2 3) '(4 5 6))
  (matrix-*-vector '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '(1 2 3 4))
  (transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
  (matrix-*-matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '((1) (2) (3) (4)))

  )

;; Exercise 2.38: The accumulate procedure is also known as fold-right, because it combines the first element of the
;; sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to
;; fold-right, except that it combines elements working in the opposite direction:
(defn fold-left [op initial sequence]
  (letfn [(iter [result s]
            (if (empty? s)
              result
              (recur (op result (first s)) (rest s))))]
    (iter initial sequence)))
(def fold-right accumulate)

;; What are the values of
;; (fold-right / 1 (list 1 2 3))
;; (fold-left / 1 (list 1 2 3))
;; (fold-right list '() (list 1 2 3))
;; (fold-left list '() (list 1 2 3))
;; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any
;; sequence.

(comment
  (println (fold-right / 1 '(1 2 3)))
  (println (fold-left / 1 '(1 2 3)))
  (println (fold-right list '() '(1 2 3)))
  (println (fold-left list '() '(1 2 3)))
  )
;; 如果op满足交换律(associative)和结合律(commutative)，那么fold-right和fold-left会产生相同的值

;; Exercise 2.39: Complete the following definitions of reverse (exercise 2.18) in terms of fold-right and fold-left from
;; exercise 2.38:
(defn reverse2 [sequence]
  (fold-right (fn [a b] (concat b (list a))) '() sequence))

(defn reverse3 [sequence]
  (fold-left (fn [a b] (clojure.core/cons b a)) '() sequence))

(comment
  (println (reverse2 '(1 2 3 4 5)))
  (println (reverse3 '(1 2 3 4 5)))
  )

;; Exercise 2.40: Define a function unique-pairs that, given an integer n, generates the sequence of pairs (i, j) with
;; 1 <= j < i <= n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.
(defn unique-pairs [n]
  (mapcat (fn [i] (map (fn [j] [i j]) (range 1 i))) (range 2 (inc n))))

(comment
  (println (unique-pairs 5))
  )

(defn prime? [n]
  (loop [i 2]
    (cond
      (> i (Math/sqrt n)) true
      (zero? (rem n i)) false
      :else (recur (inc i)))))

(defn prime-sum-pairs [n]
  (filter (fn [[i j]] (prime? (+ i j))) (unique-pairs n)))

(comment
  (println (prime-sum-pairs 5))
  )


;; Exercise 2.41: Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or
;; equal to a given integer n that sum to a given integer s.
(defn unique-triples [n]
  (for [i (range 1 (inc n))
        j (range 1 i)
        k (range 1 j)]
    [k j i]))

(defn sum-triples [n s]
  (filter (fn [[i j k]] (= s (+ i j k))) (unique-triples n)))

(comment
  (println (sum-triples 5 8))
  )


;; Exercise 2.42: The "eight-queens puzzle" asks how to place eight queens on a chessboard so that no queen is in check
;; from any other (i.e., no two queens are in the same row, column, or diagonal). One possible solution is shown in
;; figure 2.6. One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have
;; placed k - 1 queens, we must place the kth queen in a position where it does not check any of the queens already
;; on the board. We can formulate this approach recursively: Suppose that we have already generated the sequence of all
;; possible ways to place k - 1 queens in the first k - 1 columns of the board. For each of these ways, we generate an
;; extended set of positions by placing a queen in each row of the kth column. Now we filter these, keeping only the
;; positions for which the queen in the kth column is safe with respect to the other queens. This produces the sequence
;; of all ways to place k queens in the first k columns. By continuing this process, we will produce not only one
;; solution, but all solutions to the puzzle.
;; We implement this solution as a procedure queens that returns a sequence of all solutions to the problem of placing
;; n queens on an n x n chessboard. Queens has an internal procedure queen-cols that returns the sequence of all
;; positions to place a queen in the first k columns of the board.

;(define (queens board-size)
;        (define (queen-cols k)
;                (if (= k 0)
;                  (list empty-board)
;                  (filter
;                    (lambda (positions) (safe? k positions))
;                    (flatmap
;                      (lambda (rest-of-queens)
;                              (map (lambda (new-row)
;                                           (adjoin-position
;                                             new-row k rest-of-queens))
;                                   (enumerate-interval 1 board-size)))
;                      (queen-cols (- k 1))))))
;        (queen-cols board-size))
;; In this procedure, positions is a list of the positions of queens in the first k - 1 columns, and new-row is a
;; proposed row in which to place the queen for the kth column. Complete the program by implementing the representation
;; for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a
;; set of positions, and the empty-board, which represents an empty set of positions. You must also write the procedure
;; safe?, which determines for a set of positions whether the queen in the kth column is safe with respect to the others.
;; (Note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with
;; respect to each other.)

(def empty-board '())
(defn adjoin-position [new-row k rest-of-queens]
  (concat rest-of-queens (list new-row)))

(defn enumerate-interval [low high]
  (range low (inc high)))

(defn safe? [k positions]
  (let [new-row (nth positions (dec k))]
    (loop [i 0]
      (cond
        (= i (dec k)) true
        (or (= new-row (nth positions i))
            (= (Math/abs (- new-row (nth positions i))) (- k i 1))) false
        :else (recur (inc i))))))

(comment
  (safe? 2 '(1 2))
  )

(defn flatmap [f seq]
  (mapcat f seq))

(defn queen-cols [k board-size]
  (if (= k 0)
    (list empty-board)
    (filter (fn [positions] (safe? k positions))
            (flatmap (fn [rest-of-queens]
                       (map (fn [new-row]
                              (adjoin-position new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)))
                     (queen-cols (dec k) board-size)))))

(defn queens [board-size]
  (queen-cols board-size board-size))

(comment
  (count (queens 8))
  )

;; Exercise 2.43: Louis Reasoner is having great difficulty doing exercise 2.42. His queens procedure seems to work, but
;; it runs extremely slowly. When he asks Eva Lu Ator for help, she points out that he has interchanged the order of the
;; nested mappings in flatmap, writing it as
;(flatmap
;  (lambda (new-row)
;    (map
;      (lambda (rest-of-queens)
;        (adjoin-position new-row k rest-of-queens))
;      (queen-cols (- k 1))))
;  (enumerate-interval 1 board-size))
;; Explain why this interchange makes the program run slowly. Estimate how long it will take Louis's program to solve
;; the eight-queens puzzle, assuming that the program in exercise 2.42 solves the puzzle in time T.

;; This interchange makes the program run slowly because the number of ways to place k queens in the first k columns is
;; much larger than the number of ways to place k - 1 queens in the first k - 1 columns. The program will take T times
;; 8 times T to solve the eight-queens puzzle.
