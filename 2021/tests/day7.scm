(use-modules (impls)
             (srfi srfi-64))

(test-begin "day7")

(define example-input "16,1,2,0,4,2,7,1,2,14")

(define parsed-example-input '(16 1 2 0 4 2 7 1 2 14))

(test-equal parsed-example-input (parse-day7 example-input))

(test-equal 41 (position-cost  1 parsed-example-input))
(test-equal 37 (position-cost  2 parsed-example-input))
(test-equal 71 (position-cost 10 parsed-example-input))

(test-equal '(2 . 37) (new-pos-least-fuel position-cost parsed-example-input))

(test-equal 343441 (day7-part1))

(test-equal 206 (non-const-position-cost 2 parsed-example-input))
(test-equal 168 (non-const-position-cost 5 parsed-example-input))

(test-equal '(5 . 168) (new-pos-least-fuel non-const-position-cost parsed-example-input))

(test-equal 98925151 (day7-part2))

(test-end "day7")
