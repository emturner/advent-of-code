(use-modules (srfi srfi-64)
             (impls))

(test-begin "day1")

(test-equal "empty list"
  0
  (count-increases '()))

(test-equal "single elem list"
  0
  (count-increases '(1)))

(test-equal "two elem list with increase"
  1
  (count-increases '(1 5)))

(test-equal "example"
  7
  (count-increases '(199 200 208 210 200 207 240 269 260 263)))

(test-equal "sliding 3"
  0
  (count-increases-sliding '(1 2 3)))

(test-equal "sliding 4"
  1
  (count-increases-sliding '(1 2 3 4)))

(test-equal "sliding example"
  5
  (count-increases-sliding '(199 200 208 210 200 207 240 269 260 263)))


(test-end "day1")
