(use-modules (impls day10)
             (srfi srfi-64))

(test-begin "day10")

(test-equal '() (parse ""))

(test-equal '(#\)) (parse "("))

(test-equal '(#\} #\)) (parse "({"))

(test-equal '(#\} #\} #\] #\] #\) #\} #\) #\]) (parse "[({(<(())[]>[[{[]{<()<>>"))

(test-assert (char=? #\) (parse ")")))

(test-assert (char=? #\} (parse "({[{()}]})({[})")))

(define example-input '("[({(<(())[]>[[{[]{<()<>>"
                        "[(()[<>])]({[<{<<[]>>("
                        "{([(<{}[<>[]}>{[]{[(<()>"
                        "(((({<>}<{<{<>}{[]{[]{}"
                        "[[<[([]))<([[{}[[()]]]"
                        "[{[{({}]{}}([{[{{{}}([]"
                        "{<[[]]>}<{[{[{[]{()[[[]"
                        "[<(<(<(<{}))><([]([]()"
                        "<{([([[(<>()){}]>(<<{{"
                        "<{([{{}}[<[[[<>{}]]]>[]]"))

(test-equal 26397 (corrupted-score example-input))

(test-equal 288957 (completion-score '(#\} #\} #\] #\] #\) #\} #\) #\])))

(test-equal 288957 (incomplete-score example-input))

(test-equal 392097 (day10-part1))

(test-equal 4263222782 (day10-part2))

(test-end "day10")
