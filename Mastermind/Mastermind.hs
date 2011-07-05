module Mastermind where

import Data.List (intersect, (\\))


newtype Eq a => PegColors a = PegColors [a]
                            deriving (Eq, Show)

data StdPegColor = White | Yellow | Orange | Red | Green | Blue
                 deriving (Eq, Enum, Show)

stdPegColors = PegColors [White .. Blue]

letterPegColors = PegColors "abcdef"


newtype Eq a => Pattern a = Pattern [a]
                          deriving (Eq, Show)

data Score = Score { black :: Int, white :: Int }
           deriving (Eq, Show)

calcScore :: Eq a => Pattern a -> Pattern a -> Score
calcScore (Pattern target) (Pattern candidate)
  | length target /= length candidate =
      error "Target and candidate patterns must have the same length."
  | otherwise =
      Score (countBlack target candidate) (countWhite target candidate)
        where countBlack t c = length $ filter id $ zipWith (==) t c
              countWhite t c = length c - length (c \\ t) - countBlack t c


allPatterns :: Eq a => Int -> PegColors a -> [Pattern a]
allPatterns pegCount (PegColors colors) = map (Pattern . reverse)
                                            $ allPatterns' pegCount
  where allPatterns' 0        = [[]]
        allPatterns' pegCount = concatMap (\t -> map (:t) colors)
                                        $ allPatterns' (pegCount - 1)


type Strategy a = Int -> PegColors a -> (Pattern a -> Score)
                      -> [(Pattern a, Score)]


sequentialSearch :: Eq a => Strategy a
sequentialSearch pegCount pegColors askScore =
  take rowCount rows
  where rows = map (\p -> (p, askScore p))
                   (allPatterns pegCount pegColors)
        rowCount = 1 + length (takeWhile ((/= Score pegCount 0) . snd) rows)
