(define-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1))

(define inputs-dir "/home/emma/sources/advent-of-code/2021/inputs/")

(define-public (read-lines-to-list filename)
  "reads a file line by line, calling next on each line"
  (call-with-input-file (string-append inputs-dir filename)
    (lambda (file)
      (define step
        (lambda (ls)
          (let ((line (read-line file)))
            (if (eof-object? line)
                (reverse ls)
                (step (cons line ls))))))
      (step '()))))

(define-public (transpose xs)
  (apply map list xs))

(define-public (time-proc proc)
  (let* ((start (get-internal-run-time))
         (_ (proc))
         (end (get-internal-run-time))
         (diff (- end start)))
    (/ diff internal-time-units-per-second 1.0)))

(define-public (bench-proc proc)
  (/ (bench-aux proc 10000 0.0)
     10000))

(define (bench-aux proc iter total)
  (if (> iter 0)
      (bench-aux proc
                 (- iter 1)
                 (+ total (time-proc proc)))
      total))
