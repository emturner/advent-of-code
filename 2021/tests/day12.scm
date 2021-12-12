(use-modules (impls day12)
             (srfi srfi-64))

(test-begin "day12")

(define example-input '("start-A"
                        "start-b"
                        "A-c"
                        "A-b"
                        "b-d"
                        "A-end"
                        "b-end"))

(define parsed-example (parse-input example-input))

(test-equal "parsed input has 6 caves"
  6
  (hash-count (lambda (_key _value) #t) parsed-example))

(test-equal "start links to"
  '("b" "A")
  (adjacent-caves parsed-example "start"))

(test-equal "A links to"
  '("end" "b" "c" "start")
  (adjacent-caves parsed-example "A"))

(test-equal "b links to"
  '("end" "d" "A" "start")
  (adjacent-caves parsed-example "b"))

(test-equal "c links to"
  '("A")
  (adjacent-caves parsed-example "c"))

(test-equal "d links to"
  '("b")
  (adjacent-caves parsed-example "d"))

(test-equal "end links to"
  '("b" "A")
  (adjacent-caves parsed-example "end"))

(test-equal "cave not in map links to nothing"
  '()
  (adjacent-caves parsed-example "notinmap"))

(test-equal "allowed-caves"
  '("a" "B" "e")
  (filter-allowed-caves '("a" "B" "c" "e" "end") '("B" "start" "c") #f))

(define first-step-example (step parsed-example '("start") #f))

(test-equal "example first step"
  '(#f ("b" "start") ("A" "start"))
  first-step-example)

(test-equal "count paths example" 10 (count-paths parsed-example #f))

(define example-2 '("dc-end"
                    "HN-start"
                    "start-kj"
                    "dc-start"
                    "dc-HN"
                    "LN-dc"
                    "HN-end"
                    "kj-sa"
                    "kj-HN"
                    "kj-dc"))

(test-equal "count paths example 2"
  19
  (count-paths (parse-input example-2) #f))

(define example-3
  '("fs-end"
    "he-DX"
    "fs-he"
    "start-DX"
    "pj-DX"
    "end-zg"
    "zg-sl"
    "zg-pj"
    "pj-he"
    "RW-he"
    "fs-DX"
    "pj-RW"
    "zg-RW"
    "start-pj"
    "he-WI"
    "zg-he"
    "pj-fs"
    "start-RW"))

(test-equal "count paths example 3"
  226
  (count-paths (parse-input example-3) #f))

(test-equal "count paths example visit small twice"
  36
  (count-paths parsed-example #t))

(test-equal "count paths example 2 visit small twice"
  103
  (count-paths (parse-input example-2) #t))

(test-equal "count paths example 3 visit small twice"
  3509
  (count-paths (parse-input example-3) #t))

(test-equal "day12 part1"
  4720
  (day12-part1))

(test-equal "day12 part2"
  147848
  (day12-part2))

(test-end "day12")
