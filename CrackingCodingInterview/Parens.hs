-- Problem 9.6 (page 110):
-- Implement an algorithm to print all valid (e.g., properly opened and closed)
-- combinations of n-pairs of parentheses.
-- Example:
-- Input: 3
-- Output: ((())), (()()), (())(), ()(()), ()()()

import Data.List (sort)


parens :: Int -> [String]
parens 0 = [""]
parens 1 = ["()"]
parens n = concat [ parens' (n - i) (i - 1) | i <- [1..n] ]
  where parens' a b = [ "(" ++ sa ++ ")" ++ sb | sa <- parens a, sb <- parens b ]


printParens n = let ps = parens n in
                do mapM_ putStrLn ps
                   print $ length ps


main = print $ map (length . parens) [1..12]
-- [1,2,5,14,42,132,429,1430,4862,16796,58786,208012]


invertParen '(' = ')'
invertParen ')' = '('
invertParen c   = c

checkReverse n = let ps = parens n
                 in sort ps == (sort (map (reverse . map invertParen) ps))
