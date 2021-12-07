# Advent of Code
Contains my [Advent of Code](https://adventofcode.com) solutions, written in Scheme (Gnu Guile)

## Prerequisites
The prerequisites are listed in [manifest.scm](./manifest.scm). If you have [Gnu Guix](https://guix.gnu.org/) installed, you can enter a shell environment containing these prerequisites by running:
``` bash
guix shell -m manifest.scm
```

## Running tests
You can run any of the tests within the '2021' folder.
e.g. to run the tests for the first day:
``` bash
cd 2021
guile -L . tests/day1.scm
```
The logs for the test run will then be saved to `2021/day1.scm`.
