import Prelude hiding (Left, Right)
import Control.Monad
import Data.Char
import Data.List

data Fate = 
    Central | Left | Right | Dead
    deriving Show

containsZero :: String -> Bool
containsZero ss = elem '0' ss

divides :: Int -> Int -> Bool
divides x y = mod x y == 0

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = not . any (divides x) $ takeWhile (\i -> i * i <= x) [2 .. ]

isLeft :: String -> Bool
isLeft ss | containsZero ss = False
isLeft ss = all isPrime . map read . init $ tails ss

isRight :: String -> Bool
isRight ss | containsZero ss = False
isRight ss = all isPrime . map read . tail $ inits ss

isCenter :: String -> Bool
isCenter ss =
    isLeft ss && isRight ss

fate :: String -> Fate
fate ss
    | isCenter ss = Central
    | isLeft   ss = Left
    | isRight  ss = Right
    | otherwise   = Dead

main :: IO ()
main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        ss <- getLine
        putStrLn $ map toUpper $ show $ fate ss
