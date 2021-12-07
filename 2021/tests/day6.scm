(use-modules (impls day6)
             (srfi srfi-64))

(test-begin "day6")

(define example-input "3,4,3,1,2")

(define parsed-example-input
  (let* ((h (make-hash-table))
        (_  (hash-set! h 3 2))
        (_  (hash-set! h 1 1))
        (_  (hash-set! h 4 1))
        (_  (hash-set! h 2 1)))
    h))

(test-equal "parse example input count"
  4
  (hash-count (lambda (_key _value) #t) (parse-day6 example-input)))

(test-equal "parse example input 1 4s"
  1
  (hash-ref (parse-day6 example-input) 4))

(test-equal "parse example input 1 2s"
  1
  (hash-ref (parse-day6 example-input) 2))

(test-equal "parse example input 1 1"
  1
  (hash-ref (parse-day6 example-input) 1))

(test-equal "parse example input 2 3s"
  2
  (hash-ref (parse-day6 example-input) 3))

(test-equal "fishes count example after 1"
  5
  (count-fishes (fish-step-days 1 parsed-example-input)))

(test-equal "fishes count example after 2"
  6
  (count-fishes (fish-step-days 2 parsed-example-input)))

(test-equal "fishes count example after 3"
  7
  (count-fishes (fish-step-days 3 parsed-example-input)))

(test-equal "fishes count example after 18"
  26
  (count-fishes (fish-step-days 18 parsed-example-input)))

(test-equal "fishes count example after 80"
  5934
  (count-fishes (fish-step-days 80 parsed-example-input)))

(test-equal "fishes count example after 256"
  26984457539
  (count-fishes (fish-step-days 256 parsed-example-input)))

(test-equal "part 1" 390011 (day6-part1))


(test-end "day6")
