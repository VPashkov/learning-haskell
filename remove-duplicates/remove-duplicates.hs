distinct [] = []
distinct (x:xs) = x : distinct (filter (/=x) xs)

main = fmap distinct getLine >>= putStrLn