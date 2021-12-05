(use-modules (impls)
             (srfi srfi-64))

(test-begin "day5")

(define example-input
  '("0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"))

(define parsed-example-input
  '(((0 . 9) . (5 . 9))
    ((8 . 0) . (0 . 8))
    ((9 . 4) . (3 . 4))
    ((2 . 2) . (2 . 1))
    ((7 . 0) . (7 . 4))
    ((6 . 4) . (2 . 0))
    ((0 . 9) . (2 . 9))
    ((3 . 4) . (1 . 4))
    ((0 . 0) . (8 . 8))
    ((5 . 5) . (8 . 2))))

(test-equal "parse example" parsed-example-input (parse-day5 example-input))

(test-equal "removes diagonals"
  '(((0 . 9) . (5 . 9))
    ((9 . 4) . (3 . 4))
    ((2 . 2) . (2 . 1))
    ((7 . 0) . (7 . 4))
    ((0 . 9) . (2 . 9))
    ((3 . 4) . (1 . 4)))
  (remove-diagonals parsed-example-input))

(test-equal "line-points vert"
  '((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5))
  (vent-coords-pair->line '((0 . 0) . (0 . 5))))

(test-equal "line-points reducing"
  '((1 . 5) (1 . 4) (1 . 3) (1 . 2) (1 . 1) (1 . 0))
  (vent-coords-pair->line '((1 . 5) . (1 . 0))))

(test-equal "line-points horiz"
  '((5 . 8) (6 . 8) (7 . 8) (8 . 8) (9 . 8))
  (vent-coords-pair->line '((5 . 8) . (9 . 8))))

(test-equal "line-points diag"
  '((5 . 6) (6 . 7) (7 . 8) (8 . 9))
  (vent-coords-pair->line '((5 . 6) . (8 . 9))))

(test-equal "line-points diag"
  '((5 . 9) (6 . 8) (7 . 7) (8 . 6))
  (vent-coords-pair->line '((5 . 9) . (8 . 6))))

(test-equal "example part 1"
  5
  (count-vent-line-intersections
   (map vent-coords-pair->line
        (remove-diagonals parsed-example-input))))

(test-equal "example part 2"
  12
  (count-vent-line-intersections
   (map vent-coords-pair->line parsed-example-input)))

(test-equal "day5 part1" 6189 (day5-part1))
(test-equal "day5 part2" 19164 (day5-part2))

(test-end "day5")
