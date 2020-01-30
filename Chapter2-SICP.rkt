#lang sicp

(display "section 2.1")
(newline)

;; Ex 2.1 made this handle negative sign as well
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))

(define (numer r)
  (car r))

(define (denom r)
  (cdr r))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom y) (denom x))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(print-rat (make-rat (- 1) (- 7)))


(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(newline)
(display "----INTERVAL ARITHMETIC SEPARATE FILE----")

;; ex 2.17
(define (last-pair l)
  (if (= 1 (length l))
      l
      (last-pair (cdr l))))

(last-pair '(23 72 149 34))

;; ex 2.18
(define (reverse l)
  (define (reverse-iter r acc)
    (if (null? r)
        acc
        (reverse-iter (cdr r) (cons (car r) acc))))
  (reverse-iter l '()))

(reverse '(23 72 149 34))

(define (reduce predicate acc l)
  (if (null? l)
      acc
      (reduce predicate (predicate acc (car l)) (cdr l))))

(define (filter predicate l)
  (define (filter-inner inner acc)
    (if (null? inner)
        acc
        (filter-inner
         (cdr inner)
         (if (predicate (car inner))
             (cons (car inner) acc)
             acc))))

  (filter-inner l '()))





;;  (reduce (lamnbda (x acc) (if pred)) '() l))


;; 2.20
(define (same-parity . x)
  (if (odd? (car x))
      (filter odd? x)
      (filter even? x)))

;; 2.19
(define first-denomination car)





(define except-first-denomination cdr)

(define no-more? null?)

(define (cc a k)
  (cond ((= a 0) 1)
        ((or (< a 0) (no-more? k)) 0)
        (else (+ (cc a
                     (except-first-denomination k))
                 (cc (- a (first-denomination k))
                     k)))))

;;(define (count-change amount)
;;  (cc amount 5))

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(display "COINS: ")
(= (cc 100 us-coins) 292)

;; Order of oing values does not effect the answer because all cons sells are ultimately combined. Addition commutes.

;; 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(square-list '(1 2 3 4))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(square-list2 '(1 2 3 4))

;; 2.22
(define square (lambda (x) (* x x)))

(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list3 '(1 2 3 4))

;;2.24
'(1 '(2 '(3 4)))

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car (cdr (car '('(7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 '(2 '(3 '(4 '(5 '(6 7))))))))))))))))))))))))))))

;; 2.26

(define x '(1 2 3))
(define y '(4 5 6))
(append x y)
(cons x y)
(list x y)

;; 2.27
(define deep-rev-x (list (list 1 2) (list 3 4)))

(define (deep-reverse l)
  (if (null? l)
      l
      (map reverse (reverse l))))


;; 2.28
(define tree-list (list (list 1 2) (list 3 4)))

(define left car)
(define right cdr)

;; this one is silly TODO
(define (fringe tree)
  (display tree)(newline)
  (let ((left (car tree))
        (right (cdr tree)))
   (if (null? tree)
      '()
      (cons (fringe left) (fringe right)))))

(newline)
(display "----END OF SECTION----")
