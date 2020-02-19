{-# LANGUAGE OverloadedStrings #-}

-- a very simple parser for very rigid bibentries
module Habib where

import Data.Attoparsec.Text
import Data.Char

data Field
  = Address
  | Annote
  | Author
  | Booktitle
  | Chapter
  | Crossref
  | Edition
  | Editor
  | Howpublished
  | Institution
  | Journal
  | Key
  | Month
  | Note
  | Number
  | Organization
  | Pages
  | Publisher
  | School
  | Series
  | Title
  | Type
  | Volume
  | Year
  deriving (Show, Eq)

data Entrytype =
  Entrytype
    { compulsory :: [Field]
    , optional :: Maybe [Maybe Field]
    }
  deriving (Eq, Show)

article :: Entrytype
article =
  Entrytype
    { compulsory = [Author, Title, Journal, Year]
    , optional =
        Just [Just Volume, Just Number, Just Pages, Just Month, Just Note]
    }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
