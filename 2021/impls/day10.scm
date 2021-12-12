(define-module (impls day10)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public (day10-part1)
  (corrupted-score (read-lines-to-list "day10")))

(define-public (day10-part2)
  (incomplete-score (read-lines-to-list "day10")))

(define-public (incomplete-score lines)
  (let* ((parsed (map parse lines))
         (completions (filter list? parsed))
         (scores (map completion-score completions))
         (sorted (sort scores <)))
    (list-ref
     sorted
     (floor (/ (length sorted) 2)))))

(define-public (completion-score completion)
  (fold
   (lambda (c acc)
     (+ (* 5 acc)
        (match c
          (#\) 1)
          (#\] 2)
          (#\} 3)
          (#\> 4))))
   0
   completion))


(define-public (corrupted-score lines)
  (let* ((parsed (map parse lines))
         (corrupted-chars (filter char? parsed)))
    (fold
     (lambda (c acc)
       (+ acc
          (match c
            (#\) 3)
            (#\] 57)
            (#\} 1197)
            (#\> 25137))))
     0
     corrupted-chars)))

(define-public (parse line)
  (parse-aux line '()))

(define (parse-aux line expected)
  (if (string=? line "")
      expected
      (match (string-ref line 0)
        (#\( (parse-aux (substring line 1) (cons #\) expected)))
        (#\{ (parse-aux (substring line 1) (cons #\} expected)))
        (#\[ (parse-aux (substring line 1) (cons #\] expected)))
        (#\< (parse-aux (substring line 1) (cons #\> expected)))
        (c (if (and (not (eq? '() expected))
                    (char=? c (car expected)))
               (parse-aux (substring line 1) (cdr expected))
               c)))))
