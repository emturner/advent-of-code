(define-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1))

(define dir "/home/emma/sources/advent-of-code/2021/")
(define-public (read-lines-to-list filename)
  "reads a file line by line, calling next on each line"
  (call-with-input-file (string-append dir filename)
    (lambda (file)
      (define step
        (lambda (ls)
          (let ((line (read-line file)))
            (if (eof-object? line)
                (reverse ls)
                (step (cons line ls))))))
      (step '()))))
