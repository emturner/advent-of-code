(define-module (impls day5)
  #:use-module (utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

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
