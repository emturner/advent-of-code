(define-module (impls)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-sub))

;; -----
;; DAY 7
;; -----
(define-public (day7-part1)
  (let* ((lines  (read-lines-to-list "day7"))
         (positions (parse-day7 (car lines)))
         (min-pos-and-fuel (new-pos-least-fuel position-cost positions)))
    (cdr min-pos-and-fuel)))

(define-public (day7-part2)
  (let* ((lines  (read-lines-to-list "day7"))
         (positions (parse-day7 (car lines)))
         (min-pos-and-fuel (new-pos-least-fuel non-const-position-cost positions)))
    (cdr min-pos-and-fuel)))

(define-public (parse-day7 line)
  (let ((numbers (string-split line #\,)))
    (map string->number numbers)))

(define-public (new-pos-least-fuel cost-proc positions)
  (match (min-max positions)
    ((min . max)
     (let ((pos-to-try (range min max)))
       (fold
        (lambda (pos acc)
          (match acc
            ((min-pos . min-fuel)
             (let ((fuel (cost-proc pos positions)))
               (if (< fuel min-fuel)
                   (cons pos fuel)
                   acc)))))
        (cons min (cost-proc (car pos-to-try) positions))
        (cdr pos-to-try))))))

;; Each step costs 1, then 2, then 3 etc
;; ie sum of first n naturals
(define-public (non-const-position-cost new-pos positions)
  (fold (lambda (pos acc)
          (let ((diff (abs (- pos new-pos))))
            (+ acc
             (/ (* diff (+ diff 1)) 2))))
        0
        positions))

(define-public (position-cost new-pos positions)
  (fold (lambda (pos acc)
          (+ acc
             (abs (- pos new-pos))))
        0
        positions))
;; -----
;; DAY 6
;; -----
(define-public (day6-part1)
  (let* ((lines  (read-lines-to-list "day6"))
         (fish  (parse-day6 (car lines)))
         (after-80 (fish-step-days 80 fish)))
    (count-fishes after-80)))

(define-public (day6-part2)
  (let* ((lines  (read-lines-to-list "day6"))
         (fish  (parse-day6 (car lines)))
         (after-80 (fish-step-days 256 fish)))
    (count-fishes after-80)))

(define-public (count-fishes current)
  (hash-fold
   (lambda (_days count acc)
     (+ acc count))
   0
   current))

(define-public (fish-step-days days start)
  (if (> days 0)
      (fish-step-days
       (- days 1)
       (fish-step-day start))
      start))

(define (fish-step-day fishes)
  (let* ((new-table (make-hash-table))
         (_ (hash-for-each
             (lambda (days num)
               (match days
                 (0 (let* ((num-6 (or (hash-ref new-table 6) 0))
                           (_ (hash-set! new-table 6 (+ num-6 num)))
                           (_ (hash-set! new-table 8 (+ num-6 num))))
                      #t))
                 (7 (let* ((num-6 (or (hash-ref new-table 6) 0))
                           (_ (hash-set! new-table 6 (+ num-6 num))))
                      #t))
                 (d (hash-set! new-table (- d 1) num))))
             fishes)))
    new-table))



(define-public (parse-day6 input)
  (let* ((numbers (string-split input #\,))
         (numbers (map string->number numbers)))
    (fold
     (lambda (n acc)
       (let ((entry (hash-ref acc n)))
         (if entry
             (let ((_ (hash-set! acc n (+ 1 entry))))
               acc)
             (let ((_ (hash-set! acc n 1)))
               acc))))
     (make-hash-table)
     numbers)))

;; -----
;; DAY 5
;; -----
(define-public (day5-part1)
  (let* ((lines  (read-lines-to-list "day5"))
         (parsed (parse-day5 lines))
         (non-diag (remove-diagonals parsed))
         (vent-lines (map vent-coords-pair->line non-diag)))
    (count-vent-line-intersections vent-lines)))

(define-public (day5-part2)
  (let* ((lines  (read-lines-to-list "day5"))
         (parsed (parse-day5 lines))
         (vent-lines (map vent-coords-pair->line parsed)))
    (count-vent-line-intersections vent-lines)))

(define-public (count-vent-line-intersections vent-lines)
  (let ((sums-per-point (sum-vent-points-from-lines vent-lines)))
    (hash-count (lambda (key value) (< 1 value)) sums-per-point)))

(define-public (sum-vent-points-from-lines vent-lines)
  (fold sum-vent-points-in-lines (make-hash-table) vent-lines))

(define (sum-vent-points-in-lines vent-lines acc)
  (fold add-vent-point-to-sum acc vent-lines))

(define (add-vent-point-to-sum point acc)
  (let ((entry (hash-get-handle acc point)))
    (if entry
        (let ((_ (hash-set! acc point (+ 1 (cdr entry)))))
            acc)
        (let ((_ (hash-set! acc point 1)))
          acc))))

(define-public (vent-coords-pair->line coords)
  (match coords
    (((x . y1) . (x . y2))
     (map
      (lambda (y) (cons x y))
      (range y1 y2)))
    (((x1 . y) . (x2 . y))
     (map
      (lambda (x) (cons x y))
      (range x1 x2)))
    (((x1 . y1) . (x2 . y2))
     (let* ((xs (range x1 x2))
            (ys (range y1 y2))
            (xy (zip xs ys)))
       (map (lambda (xy)
              (match xy
                ((x y) (cons x y))))
            xy)))))

(define-public (remove-diagonals vent-line-coords)
  (filter non-diagonal? vent-line-coords))

(define (non-diagonal? vent-line-coord)
  (match vent-line-coord
    (((x . y1) . (x . y2))   #t)
    (((x1 . y) . (x2 . y))   #t)
    (((x1 . y1) . (x2 . y2)) #f)))

(define-public (parse-day5 lines)
  (map parse-day5-line lines))

(define (parse-day5-line line)
  (match (string-split line #\space)
    ((c1 "->" c2)
     (cons
      (parse-day5-coord c1)
      (parse-day5-coord c2)))))

(define (parse-day5-coord c)
  (match (string-split c #\,)
    ((x y)
     (cons
      (string->number x)
      (string->number y)))))

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
