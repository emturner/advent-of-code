(define-module (impls day6)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

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
