#!/usr/bin/env stack
-- stack --resolver lts-2.9 --install-ghc runghc --package turtle

import Control.Applicative
import Control.Monad
import System.IO

query m r n xs =
  m
--  if m - r >= 0
--    then xs !! (m - r)
--    else xs !! (m - r + n)

readArray :: IO [Int]
readArray = do
  line <- getLine
  return $! map read $ words line

main :: IO ()
main = do
  [n, k, q] <- readArray
  xs        <- readArray
  replicateM_ q $ do
    m <- readLn :: IO Int
    print $ query m (mod k n) n xs
