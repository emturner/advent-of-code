(define-module (impls day12)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public (day12-part1)
  (let* ((lines  (read-lines-to-list "day12"))
         (graph  (parse-input lines)))
    (count-paths graph #f)))

(define-public (day12-part2)
  (let* ((lines  (read-lines-to-list "day12"))
         (graph  (parse-input lines)))
    (count-paths graph #t)))

(define-public (count-paths graph can-visit-small-twice?)
  (length (count-paths-aux graph '() '("start") can-visit-small-twice?)))

(define (count-paths-aux graph completed path-so-far can-visit-small-twice?)
  (match (step graph path-so-far can-visit-small-twice?)
    ((maybe . '()) (if maybe (cons maybe completed) completed))
    ((maybe . incomplete)
     (let* ((completed (if maybe (cons maybe completed) completed))
            (next (fold
                   (lambda (i comp)
                     (append
                      comp
                      (if (or (not can-visit-small-twice?)
                              (and (not (big-cave? (car i)))
                                   (member (car i) (cdr i))))
                          (count-paths-aux graph '() i #f)
                          (count-paths-aux graph '() i can-visit-small-twice?))))
                   completed
                   incomplete)))
       next))))

(define-public (step graph path-so-far can-visit-small-twice?)
  (let* ((next-caves (adjacent-caves graph (car path-so-far)))
         (completion (if (member "end" next-caves) (cons "end" path-so-far) #f))
         (next-caves (filter-allowed-caves next-caves path-so-far can-visit-small-twice?))
         (incomplete (map (lambda (c) (cons c path-so-far)) next-caves)))
    (cons completion incomplete)))

(define-public (filter-allowed-caves caves path-so-far can-visit-small-twice?)
  (filter (lambda (cave)
            (and (not (string=? cave "end"))
                 (not (string=? cave "start"))
                 (or can-visit-small-twice?
                     (big-cave? cave)
                     (not (member cave path-so-far)))))
            caves))

(define-public (parse-input lines)
  (let* ((pairs (map parse-line lines))
         (graph (make-hash-table)))
    (fold set-cave-pair-adjacent! graph pairs)))

(define (parse-line line)
  (match (string-split line #\-)
    ((first second)
     (cons first second))))

(define-public (adjacent-caves graph cave)
  (or (hash-ref graph cave) '()))

(define (set-cave-pair-adjacent! cave-pair graph)
  (let* ((set! (lambda (g x y)
                 (hash-set! g x (cons y (adjacent-caves g x)))))
         (_ (set! graph (car cave-pair) (cdr cave-pair)))
         (_ (set! graph (cdr cave-pair) (car cave-pair))))
    graph))

(define (big-cave? cave)
  (char-upper-case? (string-ref cave 0)))
