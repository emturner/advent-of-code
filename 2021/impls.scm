(define-module (impls)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-sub))

;; -----
;; DAY 3
;; -----
(define-public (day3-part1)
  (let* ((readings (read-lines-to-list "input-day3"))
         (result (get-power readings)))
    result))

(define-public (day3-part2)
  (let* ((readings (read-lines-to-list "input-day3"))
         (result (get-life-support readings)))
    result))

(define-public (transpose xs)
  (apply map list xs))

(define (do-rating rate columns)
  (let* ((rating (rate columns))
         (rating (list->string rating)))
    (string->number rating 2)))

(define-public (get-power readings)
  (let* ((readings (map string->list readings))
         (columns  (transpose readings))
         (gamma    (do-rating rate-gamma columns))
         (epsilon  (do-rating rate-epsilon columns)))
    (* gamma epsilon)))

(define-public (get-life-support readings)
  (let* ((readings (map string->list readings))
         (columns  (transpose readings))
         (oxygen   (do-rating rate-oxygen columns))
         (co2      (do-rating rate-co2 columns)))
    (* oxygen co2)))

(define (rate-gamma columns)
  (map most-frequent-bit columns))

(define (rate-epsilon columns)
  (map least-frequent-bit columns))

(define (rate-oxygen list-columns)
  (reverse
   (oxygen-rating-aux '() list-columns)))

(define (rate-co2 list-columns)
  (reverse
   (co2-rating-aux '() list-columns)))

(define (oxygen-rating-aux acc list-columns)
  (if (eq? list-columns '())
      acc
      (let* ((current-col (car list-columns))
             (common-bit (most-frequent-bit current-col))
             (rows (transpose list-columns))
             (matching (filter
                        (lambda (row)
                          (eq? common-bit (car row)))
                        rows))
             (cols-matching (transpose matching)))
        (oxygen-rating-aux (cons common-bit acc)
                           (cdr cols-matching)))))

(define-public (co2-rating-aux acc list-columns)
  (if (eq? list-columns '())
      acc
      (let* ((current-col (car list-columns))
             (uncommon-bit (least-frequent-bit current-col))
             (rows (transpose list-columns))
             (matching (filter
                        (lambda (row)
                          (eq? uncommon-bit (car row)))
                        rows))
             (cols-matching (transpose matching)))
        (co2-rating-aux (cons uncommon-bit acc)
                        (cdr cols-matching)))))

(define (most-frequent-bit column)
  (let* ((col-str    (list->string column))
         (max-count  (string-length col-str))
         (num-ones   (string-count col-str #\1)))
    (if (< (* 2 num-ones)
           max-count)
        #\0
        #\1)))

(define (least-frequent-bit column)
  (let* ((col-str    (list->string column))
         (max-count  (string-length col-str))
         (num-ones   (string-count col-str #\1)))
    (if (eq? (length column) 1)
        (car column)
        (if (>= (* 2 num-ones) max-count)
            #\0
            (if (eq? num-ones 0)
                #\0
                #\1)))))

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
