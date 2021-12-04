(use-modules (impls)
             (srfi srfi-64))

(test-begin "day4")

(define example-input
  '("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    ""
    "22 13 17 11  0"
    " 8  2 23  4 24"
    "21  9 14 16  7"
    " 6 10  3 18  5"
    " 1 12 20 15 19"
    ""
    " 3 15  0  2 22"
    " 9 18 13 17  5"
    "19  8  7 25 23"
    "20 11 10 24  4"
    "14 21 16 12  6"
    ""
    "14 21 17 24  4"
    "10 16 15  9 19"
    "18  8 23 26 20"
    "22 11 13  6  5"
    " 2  0 12  3  7"))

(define parsed-example-input
  (cons
   '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1)
   '(((22 13 17 11 0)
      ( 8  2 23  4 24)
      (21  9 14 16  7)
      ( 6 10  3 18  5)
      ( 1 12 20 15 19))
     (( 3 15  0  2 22)
      ( 9 18 13 17  5)
      (19  8  7 25 23)
      (20 11 10 24  4)
      (14 21 16 12  6))
     ((14 21 17 24  4)
      (10 16 15  9 19)
      (18  8 23 26 20)
      (22 11 13  6  5)
      (2  0 12  3  7)))))

(test-equal "parse example" parsed-example-input (parse-day4 example-input))

(test-equal "play bingo round on board no win"
  '((1 2 3)
    (4 #t 6)
    (7 #t 9))
  (play-bingo-round-on-board 5 '((1 2 3) (4 5 6) (7 #t 9))))

(test-equal "play bingo round on board vert win"
  '(winner . ((1 #t 3) (4 #t 6) (7 #t 9)))
  (play-bingo-round-on-board 2 '((1 2 3) (4 #t 6) (7 #t 9))))

(test-equal "play bingo round on board row win"
  '(winner . ((1 #t 3) (#t #t #t) (7 8 9)))
  (play-bingo-round-on-board 4 '((1 #t 3) (4 #t #t) (7 8 9))))

(test-equal "play bingo example"
  '(24 . ((#t #t #t #t #t)
          (10 16 15 #t 19)
          (18  8 #t 26 20)
          (22 #t 13  6 #t)
          (#t #t 12  3 #t)))
  (play-bingo (car parsed-example-input) (cdr parsed-example-input)))

(test-equal "play loosing bingo example"
  '(13 . (( 3 15 #t #t 22)
          (#t 18 #t #t #t)
          (19  8 #t 25 #t)
          (20 #t #t #t #t)
          (#t #t #t 12  6)))
  (play-loosing-bingo (car parsed-example-input) (cdr parsed-example-input)))

(test-equal "loosing score example"
  1924
  (bingo-loosing-board-score (car parsed-example-input)
                             (cdr parsed-example-input)))

(test-equal "winning score example"
  4512
  (bingo-winning-board-score (car parsed-example-input)
                             (cdr parsed-example-input)))

(test-equal "day4-part1" 49686 (day4-part1))

(test-end "day4")
