(define-module (impls day3)
  #:use-module (utils)
  #:use-module (srfi srfi-1))

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
