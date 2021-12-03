(use-modules (srfi srfi-64)
             (impls))

(test-begin "day3")

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
