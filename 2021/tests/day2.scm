(use-modules (srfi srfi-64)
             (impls))

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
