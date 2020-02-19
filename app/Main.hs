{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Char

-- import Habib
entrytype :: Parser String
entrytype = do
  entryMarker <- char '@'
  bibentryType <- many' $ satisfy isAlpha
  return bibentryType

main :: IO ()
main = do
  let fileName = "shelah.bib"
  input <- readFile fileName
  putStrLn input
  -- either putStrLn putStrLn $ parseOnly entrytype <$> (head . lines) input
