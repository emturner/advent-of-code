(define-module (impls)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-sub))

;; -----
;; DAY 2
;; -----
(define-record-type <sub>
  (make-sub horizontal depth aim)
  sub?
  (horizontal  sub-horizontal)
  (depth       sub-depth)
  (aim         sub-aim))

(define-public (day2-part1)
  (let* ((moves (read-lines-to-list "input-day2"))
         (moves (map parse-sub-move moves))
         (sub   (sub-after-moves moves))
         (depth (sub-depth sub))
         (horiz (sub-horizontal sub)))
    (* depth horiz)))

(define-public (day2-part2)
  (let* ((moves (read-lines-to-list "input-day2"))
         (moves (map parse-sub-move moves))
         (sub   (sub-after-moves-with-aim moves))
         (depth (sub-depth sub))
         (horiz (sub-horizontal sub)))
    (* depth horiz)))

(define (parse-sub-move move)
  (match (string-split move #\space)
    ((direction distance)
     (cons (string->symbol direction)
           (string->number distance)))))

(define-public (sub-after-moves moves)
  (fold move-sub
        (make-sub 0 0 0)
        moves))

(define-public (sub-after-moves-with-aim moves)
  (fold move-sub-with-aim
        (make-sub 0 0 0)
        moves))

(define (move-sub move sub-pos)
  (match sub-pos
    (($ <sub> horizontal depth aim)
     (match move
       (('forward . x)
        (make-sub (+ horizontal x) depth aim))
       (('down . y)
        (make-sub horizontal (+ depth y) aim))
       (('up . y)
        (make-sub horizontal (- depth y) aim))))))

(define (move-sub-with-aim move sub-pos)
  (match sub-pos
    (($ <sub> horizontal depth aim)
     (match move
       (('forward . x)
        (make-sub (+ horizontal x) (+ depth (* aim x)) aim))
       (('down . y)
        (make-sub horizontal depth (+ aim y)))
       (('up . y)
        (make-sub horizontal depth (- aim y)))))))

;; -----
;; DAY 1
;; -----
(define-public (day1-part1)
  (let* ((depths (read-lines-to-list "input-day1"))
         (depths (map string->number depths)))
    (count-increases depths)))

(define-public (day1-part2)
  (let* ((depths (read-lines-to-list "input-day1"))
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
