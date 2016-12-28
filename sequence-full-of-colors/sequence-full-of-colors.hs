-- https://www.hackerrank.com/challenges/sequence-full-of-colors

import Control.Monad

isFull [] r g b y
    | r /= g          = False
    | y /= b          = False
    | otherwise       = True
isFull (c:colors) r g b y
    | abs (r - g) > 1 = False
    | abs (y - b) > 1 = False
    | c == 'R'        = isFull colors (r + 1) g b y
    | c == 'G'        = isFull colors r (g + 1) b y
    | c == 'B'        = isFull colors r g (b + 1) y
    | c == 'Y'        = isFull colors r g b (y + 1)

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        colors <- getLine
        print $ isFull colors 0 0 0 0