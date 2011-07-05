import Mastermind


strategyWithCheck :: Eq a => String -> Strategy a -> Strategy a
strategyWithCheck patternName strategy pegCount pegColors askScore =
  if length rows > 0 && snd (last rows) == Score pegCount 0
    then rows
    else error $ "Strategy could not arrive at a solution after " ++ 
                 show (length rows) ++ " guesses for " ++
                 patternName ++ "."
  where rows = strategy pegCount pegColors askScore


runStrategy :: (Eq a, Show a) => Strategy a ->
                                 Int -> PegColors a -> (Pattern a -> Score) ->
                                 IO ()
runStrategy strategy pegCount pegColors askScore =
  do mapM_ print rows
     putStrLn $ show (length rows) ++ " guesses."
  where rows = strategyWithCheck "pattern" strategy
                                 pegCount pegColors askScore


strategyForAll :: (Eq a, Show a) => Strategy a ->
                                    Int -> PegColors a ->
                                    [[(Pattern a, Score)]]
strategyForAll strategy pegCount pegColors =
  map rows targets
  where targets = allPatterns pegCount pegColors
        rows p = strategyWithCheck (show p) strategy
                                   pegCount pegColors (calcScore p)


runStrategyForAll :: (Eq a, Show a) => Strategy a ->
                                       Int -> PegColors a -> IO ()
runStrategyForAll strategy pegCount pegColors =
  do --print rowCounts
     putStrLn $ "Average: " ++ show averageCount
     putStrLn $ "Shortest: " ++ show (minimum rowCounts)
     putStrLn $ "Longest: " ++ show (maximum rowCounts)
  where rowCounts = map length $ strategyForAll strategy pegCount pegColors
        averageCount :: Double
        averageCount = fromIntegral (sum rowCounts) /
                       fromIntegral (length rowCounts)


-- main = runStrategy --sequentialSearch
                   -- (\_ _ _ -> [(Pattern "aabb", Score 4 0)])
                   -- 4 letterPegColors (calcScore (Pattern "aabb"))

main = runStrategyForAll sequentialSearch 4 letterPegColors
