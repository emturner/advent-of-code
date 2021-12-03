(use-modules (srfi srfi-64)
             (impls))

(test-begin "day3")

(test-equal "transpose single"
  '((1) (2))
  (transpose '((1 2))))

(test-equal "transpose multiple"
  '((1 4 7) (2 5 8) (3 6 9))
  (transpose '((1 2 3) (4 5 6) (7 8 9))))

(define day3-example-data
  '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))

(test-equal "part 1 example"
  198
  (get-power day3-example-data))

(test-equal "part 2 example"
  230
  (get-life-support day3-example-data))

(test-equal "part 1" 1071734 (day3-part1))
(test-equal "part 2" 6124992 (day3-part2))

(test-end "day3")

(test-begin "day2")

(test-equal "no position moves"
  (make-sub 0 0 0)
  (sub-after-moves '()))

(test-equal "position after forward move"
  (make-sub 2 0 0)
  (sub-after-moves '((forward . 2))))

(test-equal "position after example moves"
  (make-sub 15 10 0)
  (sub-after-moves '((forward . 5)
                     (down . 5)
                     (forward . 8)
                     (up . 3)
                     (down . 8)
                     (forward . 2))))

(test-equal "part 1" 1427868 (day2-part1))

(test-equal "position after example moves with aim"
  (make-sub 15 60 10)
  (sub-after-moves-with-aim '((forward . 5)
                              (down . 5)
                              (forward . 8)
                              (up . 3)
                              (down . 8)
                              (forward . 2))))

(test-equal "part 2" 1568138742 (day2-part2))

(test-end "day2")

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
