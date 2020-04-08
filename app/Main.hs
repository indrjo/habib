{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import Data.Char

-- import Habib
articleP :: Parser article
articleP = do
  _a

main :: IO ()
main = undefined
  -- let fileName = "shelah.bib"
  -- input <- readFile fileName
  -- -- putStrLn input
  -- either putStrLn putStrLn $ parseOnly entrytype $ (head . lines) input
