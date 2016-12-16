main = do
    [n, m] <- fmap (map read . words) getLine
    let nm = n * m
    print $ sum $ map (nm/) [1..nm]