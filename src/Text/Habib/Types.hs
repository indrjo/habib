
module Text.Habib.Types where

data BibEntry = BibEntry
  { unBibType   :: BibType
  , unBibLabel  :: BibLabel
  , unBibFields :: [BibField]
  } deriving (Show)

newtype BibType = BibType String
  deriving (Show)

newtype BibLabel = BibLabel String
  deriving (Show)

data BibField = BibField String String
  deriving (Show)

{-
data Field
  = Address
  | Annote
  | Author
  | Booktitle
  | Chapter
  | Crossref
  | Edition
  | Editory
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
-}
