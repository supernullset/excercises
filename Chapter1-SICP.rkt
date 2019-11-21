#lang sicp

;; Excercise 1.2

(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
   (* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))

(define (ex1-3 a b c)
  (cond ((and (>= a b) (>= b c)) (sum-squares a b)) ;; monotonically decreasing
        ((and (>= c a) (>= b a)) (sum-squares b c)) ;; monotonically increasing
        ((and (>= c b) (>= a b)) (sum-squares a c)) ;; else
        ))


(ex1-3 1 2 3)

(define ex1-4 "if b is greater than zero, a is added to b, else b is subtracted from a -> turns into b + a")


(define ex1-5 "if normal order, the function resolves to 0 in one step, otherwise (applicative) it runs forever")

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))


(define (good-enough? guess x)
  (< (abs (+ square guess) x)) 0.0001)

(sqrt 9)


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

;; section 1.2

(define (fact1 n)
  (if (= n 1)
      1
      (* n (fact1 (- n 1)))))

(define (fact n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;; ex 1.9

(define (1.9+a a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b)))) ;; recursive because it expands out to inc

(define (1.9+b a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b)))) ;; iterative because b is used as a state variable

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


;; Via substitution done on paper
;; (f n) ==> 2 * n
;; (g n) ==> 2 ^ n
;; 2 ^ (2 ^ (2 ^ 2 ......))

;; ^^ That sucked


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fast-fib n)
  (define (fib-iter a b counter)
    (if (= counter 0)
        b
        (fib-iter (+ a b) a (- counter 1))))
  (fib-iter 1 0 n))


(define (first-denomination k)
  (cond ((= k 1) 1)
        ((= k 2) 5)
        ((= k 4) 10)
        ((= k 4) 25)
        ((= k 5) 50)))

(define (cc a k)
  (cond ((= a 0) 1)
        ((or (< a 0) (= k 0)) 0)
        (else (+ (cc a
                     (- k 1))
                 (cc (- a (first-denomination k))
                     k)))))

(define (count-change amount)
  (cc amount 5))

(define (ex1-11 n)
  (cond ((< n 3) n)
        (else (+ (ex1-11 (- n 1))
                 (* 2 (ex1-11 (- n 2)))
                 (* 3 (ex1-11 (- n 3)))))))


(define (dec a)
  (- a 1))

;; r = row, c = column
(define (ex1-12 r c)
  (if (or (= c 1) (= c r))
      1
      (+ (ex1-12 (dec r) (dec c)) (ex1-12 (dec r) c))))

(define (expt b n)
  ;; simple recursive def
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-alt b n)
  ;; iterative version - keeps track of counter and running product
  (define (expt-iter b counter product)
    ;; uses the number of exponentiation as a counter
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (*+ a b)
  ;; define multiplication in terms of addition
  (if (= b 0)
      0
      (+ a (*+ a (- b 1)))))

(define (ex1-16 a b)
  ;; Design a proc that evolves an iterative exponentiation process
  ;; that uses successive squaring and uses a log number of steps, as
  ;; does fast-expt. (Hint: using the observation that (b to the n/2,
  ;; squared) = (b squared, to the n/2), keep, along with the exponent
  ;; n and base b, an additional state variable `product`, and define the
  ;; state transformation in such a way that the product ((product * b) to the n)
  ;; is unchanged from state to state. At the beginning of the process
  ;; product is taken to be 1, and the answer is given by the value of a at
  ;; the end of the process. In general, the technique of defining an
  ;; invariant quantity that remains unchanged from state to state is
  ;; a powerful way to think abou the design of iterative algorithms.
  (define (iter-exp product b n)
    (cond ((= n 0) product)
          ;; terminate if the number of exponentiation is 0, pass in 1 as a base
          ;; case
          ((even? n) (iter-exp product (square b) (/ n 2)))
          ;; If the number of exponentiation is even, then we can use
          ;; the provided math rule to simplify our expression
          (else (iter-exp (* product b) b (- n 1)))
          ;; otherwise, evaluate the product. Meaning, accumulate the
          ;; spare number into our counter, and iter-exp for the next
          ;; level of exponentiation which _will_ be even
          ))

  (iter-exp 1 a b))

(define (ex1-17 a b)
  ;; define a fast iterative version of *+
  (define (double x) (+ x x))
  (define (halve y) (/ y 2))
  (define (fast-*+ a b)
    ;; Using b as our counter
    (cond ((= b 0) 0) ;; terminate if counter == 0
          ((even? b) (double (fast-*+ a (halve b)))) ;; if b is even, we can reduce the problem by a large step in a single chunk. This reduces our counter and makes progress towards the base condition
          (else (+ a (fast-*+ a (- b 1)))))
    )
  (fast-*+ a b))


(define (ex1-18 a b)
  ;; Using the results of Exercise 1.16 and Exercise 1.17, devise a
  ;; procedure that generates an iterative process for multiplying two
  ;; integers in terms of adding, doubling, and halving and uses a
  ;; logarithmic number of steps.
  (define (russian-peasant a b acc)
;;    (if (odd? a) (write (list "a" a "b" b "acc" acc)))
    ;; purely for debugging
    (define (double x)
      ;; utility func
      (+ x x))
    (define (halve y)
      ;; quotient is like pythons flood div (//)
      (quotient y 2))

    (cond ((= a 1) (+ acc b))
          ;; if a == 1 return the running tally
          ((even? a) (russian-peasant (halve a) (double b) acc))
          ;; if a is even, skip this evaluation
          (else (russian-peasant (halve a) (double b) (+ acc b)))))

  (russian-peasant a b 0))


(define (ex1-19)
  ;; There is a clever algorithm for computing the Fibonacci numbers
  ;; in a logarithmic number of steps. Recall the transformation of
  ;; the state variables a and b in the fib-iter process of 1.2.2: a ←
  ;; a + b and b ← a . Call this transformation T , and observe that
  ;; applying T over and over again n times, starting with 1 and 0,
  ;; produces the pair Fib ( n + 1 ) and Fib ( n ) . In other words,
  ;; the Fibonacci numbers are produced by applying T n , the n th
  ;; power of the transformation T , starting with the pair (1,
  ;; 0). Now consider T to be the special case of p = 0 and q = 1 in a
  ;; family of transformations T p q , where T p q transforms the pair
  ;; ( a , b ) according to a ← b q + a q + a p and b ← b p + a q
  ;; . Show that if we apply such a transformation T p q twice, the
  ;; effect is the same as using a single transformation T p ′ q ′ of
  ;; the same form, and compute p ′ and q ′ in terms of p and q . This
  ;; gives us an explicit way to square these transformations, and
  ;; thus we can compute T n using successive squaring, as in the
  ;; fast-expt procedure. Put this all together to complete the
  ;; following procedure, which runs in a logarithmic number of steps:

  (define (fib n)
    (fib-iter 1 0 0 1 n))

  (define (fib-iter a b p q count)
    (cond ((= count 0)
           b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* 2 (* p q)) (* q q))
                     (/ count 2)))
          (else
           (fib-iter (+ (* b q)
                        (* a q)
                        (* a p))
                     (+ (* b p)
                        (* a q))
                     p
                     q
                     (- count 1)))))

  (fib 10))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define ex1-20 "18 operations in normal order, 4 in applicative")

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                      (square (expmod base (/ exp 2) m))
                      m))
        (else (remainder
               (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (ex1-21)

  (list (smallest-divisor 199) (smallest-divisor 1999) (smallest-divisor 19999)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (ex1-22)
  (define (search-for-primes r e)
  ;; assume that r < e and is integer
  (cond ((= r e) (newline) (display "done"))
        ((even? r) (search-for-primes (+ r 1) e))
        (else (timed-prime-test r)
              (search-for-primes (+ r 1) e))))

  (search-for-primes 1000 1500))

(define ex1-25 "Technically the results would be correct, but the
remainder call in the original implementation keeps our computation
numbers smaller, and faster")


(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  ;; ^^ casually introduce
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (section-1.3.1)
;; TODO: Actually complete this
;;(define (ex-1.29)
  ;; Exercise 1.29: Simpson’s Rule is a more accurate method of
;;  numerical integration than the method illustrated above. Using
;;  Simpson’s Rule, the integral of a function f between a and b is
;;  approximated as h 3 ( y 0 + 4 y 1 + 2 y 2 + 4 y 3 + 2 y 4 + ⋯ + 2
;;  y n − 2 + 4 y n − 1 + y n ) , where h = ( b − a ) / n , for some
;;  even integer n , and y k = f ( a + k h ) . (Increasing n increases
;;  the accuracy of the approximation.) Define a procedure that takes
;;  as arguments f , a , b , and n and returns the value of the
;;  integral, computed using Simpson’s Rule. Use your procedure to
;;  integrate cube between 0 and 1 (with n = 100 and n = 1000 ), and
;;  compare the results to those of the integral procedure shown
;;  above.
;;  (define (simpsons-integral f a b n)
;;    ;; n must be even
;;    (define h (/ (- b a) n))
;;    (define yk (f (+ a (* n h))))
;;
;;    )
;;
;;  (simpsons-integral cube 0 1 100))
;; )

(define (ex1-30)
  (define (iter-sum term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (+ (term a) result))))
    (iter a 0))

  (define (sum-cubes a b)
    (iter-sum cube a inc b))

  (= (sum-cubes 1 10) 3025))

(define (ex1-31)
  (define (product f a next b)
    (if (> a b)
        1
        (* (f a) (product f (next a) next b))))

  (define (iter-product f a next b)
    (define (iter a acc)
      (if (> a b)
          acc
          (iter (next a) (* (next a) acc))))
    (iter a 1))

  (define (id x) x)

  (define (fact n)
    (product id 1 inc n))


  (define (find-pi n)
    (define (term n)
      ;; define each term in the product with relation to its sequence
      ;; position
      (if (even? n)
          (/ (+ 2 n) (+ 1 n))
          (/ (+ 1 n) (+ 2 n))))
    ;; take the product of the first 6 terms and multiply by four for
    ;; an approximation to pi
    (* (product term 1 inc 6) 4))

  ;; ^^ Wrong
  (display (find-pi 100))
  (newline)
  (display (iter-product id 1 inc 4)))

(define (ex1-32)
  (define (accumulate c base term a next b)
    (if (> a b) base
        (c (term a) (accumulate c base term (next a) next b))))

  (define (iter-accumulate c base term a next b)
    ;; define an internal iteration/aggregation function
    (define (iter a acc)
      (if (> a b)
          acc
          (iter (next a) (c acc (term a)))))

      (iter a base))

  (define (id x) x)

  (define (accusum term a next b)
    (accumulate + 0 term a next b))

  (define (accuproduct term a next b)
    (iter-accumulate * 1 term a next b))

  (accusum id 0 inc 4)
  (accuproduct id 1 inc 4)
  )

(define (ex1-33)
  (define (iter-filtered-accumulate c f base term a next b)
    ;; define an internal iteration/aggregation function
    (define (iter a acc)
      (cond ((> a b) acc)
            ;; rely on (f a) evaluating to boolean
            ((f a) (iter (next a) (c acc (term a))))
            (else (iter (next a) acc))))

      (iter a base))

  (define (relatively-prime? x y)
    (= (gcd x y) 1))

  (define (relatively-prime-to-10? x)
    (relatively-prime? x 10))

  (define (id x) x)

  (display (iter-filtered-accumulate + prime? 0 square 1 inc 5))
  ;; this wont work unless prime? is in memory
  (newline)
  (display (iter-filtered-accumulate * relatively-prime-to-10? 1 id 1 inc 10))
  )

;; TODO: come up with a better module and diplay system for my results
(ex1-33)
)

(define (section-1.3.2)
  (display "section 1.3.2")
  (newline)
  (define (pi-sum a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
         a
         (lambda (x) (+ x 4))
         b))

  (define (integral f a b dx)
    (* (sum f (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b)
       dx))


  (define (ex1-34)
    (define (f g) (g 2))
    (f (lambda (x) (* x x)))
    (f (lambda (x) (* x (+ x 1))))
    ;; (f f) => (f 2) -> (2 2)
    "1.34: if we as for f of f, via substitution we will attempt to eval (2 2), which is non sensical")

  (display (ex1-34)))

(define (section-1.3.3)
  (display "----START OF SECTION----")
  (newline)
  (display "Section 1-3.3.3")
  (newline)

  (define (search f neg-point pos-point)
    (let ((midpoint
           (average neg-point pos-point)))
      (if (close-enough? neg-point pos-point)
          midpoint
          (let ((test-value (f midpoint)))
            (cond
              ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))

  (define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
      (cond ((and (negative? a-value)
                  (positive? b-value))
             (search f a b))
            ((and (negative? b-value)
                  (positive? a-value))
             (search f b a))
            (else
             (error "Values are not of
                   opposite sign" a b)))))

  (display "ex half-interval-method:  ")
  (display (half-interval-method sin 2.0 4.0))
  (newline)

  (define tolerance 0.00001)

  (define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2))
         tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (display "guess: ")
        (display next)
        (newline)
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))

  (define (sqrt-fixed x)
    (fixed-point
     (lambda (y) (average y (/ x y)))
     1.0))

  (define (ex1-35)
    (define golden-ratio
      (fixed-point
       (lambda (x) (+ 1 (/ 1 x)))
       1.0))

    golden-ratio)

  (display "ex1-35:  ")
  (display (ex1-35))
  (newline)

  (define (ex1-36)
    (define x-to-x
      (fixed-point
       (lambda (x) (/ (log 1000) (log x))) 10))

    x-to-x)

  (display "ex1-36:  ")
  (display (ex1-36))
  (newline)

  (display "----END OF SECTION----"))
