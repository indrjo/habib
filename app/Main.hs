{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Char

-- import Lib
entrytype :: Parser String
entrytype = do
  entryMarker <- char '@'
  bibentryType <- many' $ satisfy isAlpha
  return bibentryType

main :: IO ()
main = print $ parseOnly entrytype "@article{}"
