import Mastermind


runStrategy :: (Eq a, Show a) => Strategy a ->
                                 Int -> PegColors a -> (Pattern a -> Score) ->
                                 IO ()
runStrategy strategy pegCount pegColors askScore =
  if length rows > 0 && snd (last rows) == Score pegCount 0
    then do mapM_ print rows
            putStrLn $ show (length rows) ++ " guesses."
    else error $ "Strategy could not arrive at a solution after " ++ 
                 show (length rows) ++ " guesses."
  where rows = (strategy pegCount pegColors askScore)


strategyForAll :: (Eq a, Show a) => Strategy a ->
                                    Int -> PegColors a ->
                                    [[(Pattern a, Score)]]
strategyForAll strategy pegCount pegColors =
  map rows targets
  where targets = allPatterns pegCount pegColors
        rows p = let rs = strategy pegCount pegColors (calcScore p) in
                 if length rs > 0 && snd (last rs) == Score pegCount 0
                   then rs
                   else error $ "Strategy could not arrive at a solution after " ++ 
                                show (length rs) ++ " guesses for " ++
                                show p

                                
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
