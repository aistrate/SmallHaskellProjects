module Mastermind where

import Data.List (sort, intersect)
import Control.Monad (liftM)


data Score = Score { black :: Int, white :: Int }
           deriving (Eq, Show)

newtype Colors a = Cs [a]
                 deriving (Eq, Show)

newtype Pattern a = P [a]
                  deriving (Eq, Show)


score :: (Eq a, Ord a) => Pattern a -> Pattern a -> Score
score (P target) (P cand)
  | length target /= length cand =
      error "Target and candidate patterns have different lengths."
  | otherwise =
      Score (countBlack target cand) (countWhite target cand)
        where countBlack t c = length $ filter id $ zipWith (==) t c
              countWhite t c = length (intersect t c) - countBlack t c


liftAskScore :: Show a => (Pattern a -> Score) -> (Pattern a -> IO Score)
liftAskScore askScore pattern = do let s = askScore pattern
                                   putStrLn $ (show pattern) ++ " :    " ++ (show s)
                                   return s

liftAskScore_silent :: Show a => (Pattern a -> Score) -> (Pattern a -> IO Score)
liftAskScore_silent askScore pattern = return $ askScore pattern


data Letters = A | B | C | D | E | F | G | H
             deriving (Eq, Ord, Enum, Show)

origColors :: Colors Letters
origColors = Cs [A .. F]

origPegs = 4


testingTarget = P [B, A, D, C]

testOneTarget pegs colors play target =
  do guesses <- play pegs colors (liftAskScore $ score target)
     putStrLn $ (show $ 1 + length guesses) ++ " guesses."

testAllTargets pegs colors play = do
  lengths <- mapM solveFor (solutionSpace pegs colors)
  print lengths
  putStrLn $ "Maximum: " ++ show (maximum lengths)
  let average = sum (map fromIntegral lengths :: [Double]) / fromIntegral (length lengths)
  putStrLn $ "Average: " ++ show average
  where solveFor target = do guesses <- play pegs colors (liftAskScore_silent $ score target)
                             return (1 + length guesses)

main = do
  testOneTarget origPegs origColors seqSearchPlay testingTarget
  -- testAllTargets origPegs origColors seqSearchPlay


-- Various strategies:

-- Do nothing
emptyPlay :: Int -> Colors a -> (Pattern a -> IO Score) -> IO [Pattern a]
emptyPlay pegs colors askScore = return []


-- Sequential search
seqSearchPlay :: Int -> Colors a -> (Pattern a -> IO Score) -> IO [Pattern a]
seqSearchPlay pegs colors askScore = takeWhileM isNotSol (solutionSpace pegs colors)
  where isNotSol = liftM not . isSol
        isSol p = do s <- askScore p
                     return (s == Score pegs 0)

solutionSpace :: Int -> Colors a -> [Pattern a]
solutionSpace pegs (Cs colors) = map P $ solutionSpace' pegs
  where solutionSpace' 0    = [[]]
        solutionSpace' pegs = concatMap (\t -> map (:t) colors) $ solutionSpace' (pegs - 1)

takeWhileM          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ []     =  return []
takeWhileM p (x:xs) =  do
  flg <- p x
  if flg
    then do ys <- takeWhileM p xs
            return (x:ys)
    else return []


-- Six-guess algorithm (Wikipedia)
sixGuessAlgPlay :: Int -> Colors a -> (Pattern a -> IO Score) -> IO [Pattern a]
sixGuessAlgPlay pegs colors askScore = undefined


-- Five-guess algorithm (Wikipedia)
fiveGuessAlgPlay :: Int -> Colors a -> (Pattern a -> IO Score) -> IO [Pattern a]
fiveGuessAlgPlay pegs colors askScore = undefined
