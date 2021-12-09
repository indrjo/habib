{-# LANGUAGE OverloadedStrings #-}

-- a very simple parser for very rigid bibentries
module Habib where

import qualified Data.Attoparsec.Text as A
import Data.Char
import qualified Data.Text as T

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
    { req :: [Field]
    , opt :: Maybe [Maybe Field]
    }
  deriving (Eq, Show)

article :: Entrytype
article =
  Entrytype
    { req = [Author, Title, Journal, Year]
    , opt =
        Just [Just Volume, Just Number, Just Pages, Just Month, Just Note]
    }

at :: A.Parser Char
at = A.char '@'

bgroup :: A.Parser Char
bgroup = A.char '{'

egroup :: A.Parser Char
egroup = A.char '}'

entryType :: A.Parser T.Text
entryType = at *> bibType <* bgroup

bibType :: A.Parser T.Text
bibType = A.takeWhile (/= '{')

someFunc :: IO ()
someFunc = putStrLn "someFunc"
