(define-module (impls day7)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

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
