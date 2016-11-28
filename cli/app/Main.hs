{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.Byline

main :: IO ()
main = void $ runByline $ do
  sayLn "Lats kick this off"
