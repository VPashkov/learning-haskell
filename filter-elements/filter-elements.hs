-- https://www.hackerrank.com/challenges/filter-elements

import Control.Monad
import Data.List
import Data.Ord

prettyPrint =
    putStrLn . unwords . map show

sortOn f =
    sortBy (comparing f)

defaultIfEmpty xs =
    if null xs then [-1] else xs

sortByIndex =
    sortOn (fst)

sortByValue =
    sortOn (snd)

filterByFrequency k =
    filter (\x -> length x >= k)

groupByFrequency =
    groupBy (\x y -> snd x == snd y)

zipWithIndex = zip [0..]

readArray =
    fmap (map read . words) getLine

main = do
    t <- readLn
    replicateM_ t $ do
        [n, k] <- readArray
        a <- readArray
        prettyPrint
            $ defaultIfEmpty
            $ map snd
            $ sortByIndex
            $ map head
            $ filterByFrequency k
            $ groupByFrequency
            $ sortByValue
            $ zipWithIndex a