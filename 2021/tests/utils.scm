(use-modules (srfi srfi-64)
             (utils))

(test-begin "transpose")

(test-equal "single"
  '((1) (2))
  (transpose '((1 2))))

(test-equal "multiple"
  '((1 4 7) (2 5 8) (3 6 9))
  (transpose '((1 2 3) (4 5 6) (7 8 9))))

(test-end "transpose")

(test-begin "range")

(test-equal "produces range"
  '(1 2 3 4 5 6 7 8 9)
  (range 1 9))

(test-equal "produces range descending"
  '(9 8 7 6 5 4 3 2 1)
  (range 9 1))

(test-end "range")
