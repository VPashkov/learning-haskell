import Data.List

uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

diff [] ys ds = ds ++ ys
diff xs [] ds = ds ++ xs
diff (x:xs) (y:ys) ds =
    if x == y
        then diff xs ys ds
        else diff (x:xs) ys (ds ++ [y])

main = do
    n  <- readLn :: IO Int
    xs <- fmap (sort . map read . words) getLine :: IO [Int]
    m  <- readLn :: IO Int
    ys <- fmap (sort . map read . words) getLine :: IO [Int]
    putStrLn $ unwords $ map show $ uniq $ diff xs ys []
