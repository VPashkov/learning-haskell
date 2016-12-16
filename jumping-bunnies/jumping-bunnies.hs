main = do
    n <- readLn :: IO Int
    bunnies <- fmap (map read . words) getLine
    print $ foldl1 lcm bunnies