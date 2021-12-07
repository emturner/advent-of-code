(define-module (impls day2)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-sub))

(define-record-type <sub>
  (make-sub horizontal depth aim)
  sub?
  (horizontal  sub-horizontal)
  (depth       sub-depth)
  (aim         sub-aim))

(define-public (day2-part1)
  (let* ((moves (read-lines-to-list "day2"))
         (moves (map parse-sub-move moves))
         (sub   (sub-after-moves moves))
         (depth (sub-depth sub))
         (horiz (sub-horizontal sub)))
    (* depth horiz)))

(define-public (day2-part2)
  (let* ((moves (read-lines-to-list "day2"))
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
