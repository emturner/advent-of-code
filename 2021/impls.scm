(define-module (impls)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-sub))

;; -----
;; DAY 4
;; -----
(define-public (day4-part1)
  (let* ((lines  (read-lines-to-list "day4"))
         (parsed (parse-day4 lines)))
    (match parsed
      ((numbers . boards)
       (bingo-winning-board-score numbers boards)))))

(define-public (day4-part2)
  (let* ((lines  (read-lines-to-list "day4"))
         (parsed (parse-day4 lines)))
    (match parsed
      ((numbers . boards)
       (bingo-loosing-board-score numbers boards)))))

(define-public (parse-day4 lines)
  (let* ((numbers (car lines))
         (numbers (string-split numbers #\,))
         (numbers (map string->number numbers))
         (boards (parse-boards (cddr lines))))
    (cons numbers boards)))

(define (parse-boards lines)
  (reverse
   (parse-boards-aux '() '() lines)))

(define (parse-boards-aux boards current-board lines)
  (let* ((row (string-split (car lines) #\space))
         (row (map string->number row))
         (row (filter (lambda (x) x) row))
         (current-board (cons row current-board)))
    (match (cdr lines)
      (()
       (cons (reverse current-board) boards))
      (("" ls ...)
       (parse-boards-aux (cons (reverse current-board) boards) '() ls))
      ((ls ...)
       (parse-boards-aux boards current-board ls)))))

(define-public (bingo-winning-board-score numbers boards)
  (match (play-bingo numbers boards)
    ((winning-num . winning-board)
     (* winning-num (board-score winning-board)))))

(define-public (bingo-loosing-board-score numbers boards)
  (match (play-loosing-bingo numbers boards)
    ((final-num . loosing-board)
     (* final-num (board-score loosing-board)))))

(define (board-score board)
  (let ((row-scores (map row-score board)))
    (fold + 0 row-scores)))

(define (row-score row)
  (let ((row-numbers (filter number? row)))
    (fold + 0 row-numbers)))

(define-public (play-bingo numbers boards)
  (match (play-bingo-round (car numbers) boards)
    (('winner . winning-board)
     (cons (car numbers) winning-board))
    ((boards ...)
     (match (cdr numbers)
       (() #f)
       ((ns ...) (play-bingo ns boards))))))

(define-public (play-loosing-bingo numbers boards)
  (match (play-loosing-bingo-round (car numbers) boards)
    (('winner . winning-board)
     (cons (car numbers) winning-board))
    ((loosing-board)
     ;; Only one board left! so now we want to
     ;; find its final state when it wins
     (play-bingo (cdr numbers) (list loosing-board)))
    ((boards ...)
     (match (cdr numbers)
       (() #f)
       ((ns ...) (play-loosing-bingo ns boards))))))

(define (play-loosing-bingo-round number boards)
  (let* ((new-boards (map
                      (lambda (b)
                        (play-bingo-round-on-board number b))
                      boards))
         (loosers (filter
                   (lambda (board)
                     (not (eq? 'winner (car board))))
                   new-boards)))
    loosers))

(define (play-bingo-round number boards)
  (let* ((new-boards (map
                      (lambda (b)
                        (play-bingo-round-on-board number b))
                      boards))
         (winners (filter
                   (lambda (board)
                     (eq? 'winner (car board)))
                   new-boards)))
    (if (eq? '() winners)
        new-boards
        (car winners))))

(define-public (play-bingo-round-on-board number board)
  (let ((board (map
                (lambda (row)
                  (map
                   (lambda (val)
                     (if (eq? val number) #t val))
                   row))
                board)))
    (if (board-wins? board)
        (cons 'winner board)
        board)))

(define (board-wins? board)
  (or (board-wins-rowwise? board)
      (board-wins-colwise? board)))

(define (board-wins-rowwise? board)
  (any
   (lambda (row)
     (all?
      (lambda (val)
        (eq? #t val))
      row))
   board))

(define (board-wins-colwise? board)
  (board-wins-rowwise? (transpose board)))


;; -----
;; DAY 3
;; -----
(define-public (day3-part1)
  (let* ((readings (read-lines-to-list "day3"))
         (result (get-power readings)))
    result))

(define-public (day3-part2)
  (let* ((readings (read-lines-to-list "day3"))
         (result (get-life-support readings)))
    result))

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
         (oxygen   (do-rating rate-oxygen readings))
         (co2      (do-rating rate-co2 readings)))
    (* oxygen co2)))

(define (rate-gamma columns)
  (map most-frequent-bit columns))

(define (rate-epsilon columns)
  (map least-frequent-bit columns))

(define (rate-oxygen list-rows)
  (reverse
   (oxygen-rating-aux '() list-rows)))

(define (rate-co2 list-rows)
  (reverse
   (co2-rating-aux '() list-rows)))

(define (oxygen-rating-aux acc list-rows)
  (if (eq? (car list-rows) '())
      acc
      (let* ((current-col (map car list-rows))
             (common-bit (most-frequent-bit current-col))
             (matching (filter
                        (lambda (row)
                          (eq? common-bit (car row)))
                        list-rows)))
        (oxygen-rating-aux (cons common-bit acc)
                           (map cdr matching)))))

(define-public (co2-rating-aux acc list-rows)
  (if (eq? (car list-rows) '())
      acc
      (let* ((current-col (map car list-rows))
             (uncommon-bit (least-frequent-bit current-col))
             (matching (filter
                        (lambda (row)
                          (eq? uncommon-bit (car row)))
                        list-rows)))
        (co2-rating-aux (cons uncommon-bit acc)
                        (map cdr matching)))))

(define (count-ones xs)
  (fold (lambda (x acc)
          (if (eq? x #\1)
              (+ 1 acc)
              acc))
        0
        xs))

(define (most-frequent-bit column)
  (let* ((max-count  (length column))
         (num-ones   (count-ones column)))
    (if (< (* 2 num-ones)
           max-count)
        #\0
        #\1)))

(define (least-frequent-bit column)
  (let* ((max-count  (length column))
         (num-ones   (count-ones column)))
    (if (eq? max-count 1)
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

;; -----
;; DAY 1
;; -----
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
