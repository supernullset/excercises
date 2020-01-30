#lang sicp


;; Electrical engineers will be using Alyssa’s system to compute
;; electrical quantities. It is sometimes necessary for them to compute
;; the value of a parallel equivalent resistance R p of two resistors R 1
;; and R 2 using the formula
;;
;; R p = 1 / (1 / R1 + 1 / R2)

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (neg? x) (> 0 x))

(define (pos? x) (not (neg? x)))

(define (empty? x) (= 0 (length x)))

(define (reduce predicate acc l)
  (if (empty? l)
      acc
      (reduce predicate (predicate acc (car l)) (cdr l))))

(define (all? predicate l)
  (if (reduce (lambda (acc x) (and acc (predicate x))) true l)
      true
      false))

(define (mul-interval2 x y)
  ;; let intervals == [a b], [c d]
  ;; case 1 -a  b  c  d
  ;; case 2 -a -b  c  d
  ;; case 3 -a -b -c  d
  ;; case 4 -a -b -c -d
  ;; case 5  a -b  c  d
  ;; case 6  a -b -c  d
  ;; case 7  a -b -c -d
  ;; case 8  a  b -c  d
  ;; case 9  a  b -c -d
  ;; case10  a  b  c -d
  (case (()
         )))













(define (div-interval x y)
  (if (or (= 0 (width-interval x)) (= 0 (width-interval y)))
      (error "Division error (interval spans 0)" y)
      (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define lower-bound car)

(define upper-bound cdr)

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (sub-test)
  (let ((i1 (make-interval 1 1))
        (i2 (make-interval 0 1))
      (i3 (make-interval -1 0)))
    (sub-interval i3 i1)))


(define (width-interval i)
  (abs (/ (- (upper-bound i) (lower-bound i)) 2)))

(define i1 (make-interval 1 1))
(define i2 (make-interval 0 1))
(define i3 (make-interval -1 0))

(define i4 (make-interval -1 1))
(define i5 (make-interval 4 9))

(define (width-proof)
  (display "addition: ")
  (display (=
            (+ (width-interval i1) (width-interval i2))
            (width-interval (add-interval i1 i2))
            ))

  (newline)
  (display "subtraction: ")
  (display (=
            ;; width should be a scalar, no need for direction here
            (abs (- (width-interval i1) (width-interval i2)))
            (width-interval (sub-interval i1 i2))
            ))

  (newline)
  (display "multiplication: ")
  (display (=
            ;; width should be a scalar, no need for direction here
            (abs (- (width-interval i2) (width-interval i3)))
            (width-interval (mul-interval i2 i3))
            ))

  (newline)
  (display "division: ")
  (display (=
            ;; width should be a scalar, no need for direction here
            (abs (- (width-interval i4) (width-interval i5)))

            ;; Note that unit intervals dont play nice with the division algorithm here - div by zero is possible
            (width-interval (div-interval i4 i5))
            ))
  )
