import Data.List
import Data.Maybe
import Control.Monad

isPalindrome s = s == reverse s

remove i xs = take i xs ++ drop (i + 1) xs

palindromeIndex i (s:ss) (r:rs) xs =
    if s == r
        then palindromeIndex (i + 1) ss rs xs
        else if isPalindrome (remove i xs)
            then i
            else (length xs) - i - 1

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        s <- getLine
        if isPalindrome s
            then print $ -1
            else print $ palindromeIndex 0 s (reverse s) s 