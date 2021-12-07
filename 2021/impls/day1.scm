(define-module (impls day1)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public (day1-part1)
  (let* ((depths (read-lines-to-list "day1"))
         (depths (map string->number depths)))
    (count-increases depths)))

(define-public (day1-part2)
  (let* ((depths (read-lines-to-list "day1"))
         (depths (map string->number depths)))
    (count-increases-sliding depths)))

(define-public (count-increases xs)
  (match xs
    ((x xs ...)
     (car
      (fold count-increases-step (cons 0 x) xs)))
    (_
     0)))

(define (count-increases-step current acc)
  (match acc
    ((count . prev)
     (if (> current prev)
         (cons (+ 1 count) current)
         (cons count current)))))

(define-public (count-increases-sliding xs)
  (match xs
    ((x y z xs ...)
     (car
      (fold-sliding-3 (lambda (x y z acc)
                        (count-increases-step (+ x y z) acc))
                      (cons 0 (+ x y z))
                      (cons y (cons z xs)))))
    (_ 0)))

(define (fold-sliding-3 proc acc xs)
  (match xs
    ((x y z xs ...)
     (fold-sliding-3 proc (proc x y z acc) (cons y (cons z xs))))
    (_ acc)))
