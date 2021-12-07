(define-module (impls day4)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))


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
