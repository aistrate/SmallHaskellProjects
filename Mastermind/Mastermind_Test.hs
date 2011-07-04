import Test.HUnit
import Text.Printf
import Mastermind

main = do runTestTT tests
          return ()

tests = test [
  "score_tests" ~: score_tests
  ]

score_tests = test [
  score_test "aabb" "abcd" (1, 1),
  score_test "abcd" "aabb" (1, 1),
  score_test "aaabbb" "abcade" (1, 2),
  score_test "abcdef" "aaabbb" (1, 1),
  score_test "aabbcc" "cbacba" (0, 6),
  score_test "abacad" "befaaa" (1, 3)
  ]

score_test t c (b, w) =
  printf "(%s, %s)" t c ~:
  score t c ~?= Score b w
