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
