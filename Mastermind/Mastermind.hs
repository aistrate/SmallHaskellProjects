module Mastermind where

import Data.List (intersect, (\\))


newtype Eq a => PegColors a = PegColors [a]
                            deriving (Eq, Show)

stdPegColors = PegColors "abcdef"


type Pattern a = [a]

data Score = Score { black :: Int, white :: Int }
           deriving (Eq, Show)

score :: Eq a => Pattern a -> Pattern a -> Score
score target cand
  | length target /= length cand =
      error "Target and candidate patterns must have the same length."
  | otherwise =
      Score (countBlack target cand) (countWhite target cand)
        where countBlack t c = length $ filter id $ zipWith (==) t c
              countWhite t c = length c - length (c \\ t) - countBlack t c
