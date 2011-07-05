import Test.HUnit
import Text.Printf
import Mastermind

main = do runTestTT tests
          return ()

tests = test [
  "calcScore_tests" ~: calcScore_tests
  ]

calcScore_tests = test [
  calcScore_test "aabb" "abcd" (1, 1),
  calcScore_test "abcd" "aabb" (1, 1),
  calcScore_test "aaabbb" "abcade" (1, 2),
  calcScore_test "abcdef" "aaabbb" (1, 1),
  calcScore_test "aabbcc" "cbacba" (0, 6),
  calcScore_test "abacad" "befaaa" (1, 3)
  ]

calcScore_test t c (b, w) =
  printf "(%s, %s)" t c ~:
  calcScore (Pattern t) (Pattern c) ~?= Score b w
