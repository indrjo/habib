
module Text.Habib.Parsers where

{-
  Read:
  https://metacpan.org/dist/Text-BibTeX/view/btparse/doc/bt_language.pod
-}

-- import qualified Data.Text as T
-- import qualified Data.Attoparsec.Text as A

{-| ISSUES:

For instance, the parser 'bibEntry' fails to parse the entry

@article{GRAY1980127,
  title = {Closed categories, lax limits and homotopy limits},
  author = {Gray, J.W.},
  year = 1980,
  journal = {Journal of Pure and Applied Algebra},
  publisher = {Springer-Verlag},
  address = {Berlin},
  volume = 19,
  pages = {127--158},
  doi = {https://doi.org/10.1016/0022-4049(80)90098-5},
  issn = {0022-4049},
  url = {\href{http://dx.doi.org/10.1016/0022-4049(80)90098-5}{\textsc{doi}: 0022-4049(80)90098-5}},
  note = {Lecture Notes in Mathematics, Vol. 391},
  fjournal = {Journal of Pure and Applied Algebra}
}

The issue is the field 'url = {\href{...}{...}}', where nested curly braces
occur. Of course, the braces we care here are the outer ones, whereas the
inner are to enclose the arguments of the TeX macros.
-}

import Text.Parsec
import Text.Parsec.String

import Text.Habib.Types

bibEntries :: Parser [BibEntry]
bibEntries = betweenSpaces bibEntry `sepEndBy` spaces

bibEntry :: Parser BibEntry
bibEntry = do
  name <- char '@' >> oneOf' bibEntryTypes
  betweenBrakets $ do
    tag <- many1 (noneOf " ,")
    spaces >> char ',' >> spaces
    fields <- betweenSpaces bibField `sepEndBy` char ','
    return $ BibEntry (BibType name) (BibLabel tag) fields

bibField :: Parser BibField
bibField = do
  key <- many1 $ noneOf " =\n"
  spaces >> char '=' >> spaces
  val <- bibValue
  return $ BibField key val

bibValue :: Parser String
bibValue = try (char '{' >> go 0) <|> many1 (noneOf " {},\n")
  where
    go :: Int -> Parser String
    go n = do
      c <- anyChar
      case c of
        '{' -> (c:) <$> go (n+1)
        '}' -> if n <= 0 then return "" else (c:) <$> go (n-1)
        _   -> (c:) <$> go n

bibEntryTypes :: [String]
bibEntryTypes = [ "article"
                , "booklet"
                , "book"
                , "conference"
                , "inbook"
                , "incollection"
                , "inproceedings"
                , "manual"
                , "mastersthesis"
                , "misc"
                , "mvbook"
                , "phdthesis"
                , "proceedings"
                , "techreport"
                , "unpublished"
                ]


-- Helpers

oneOf' :: [String] -> Parser String
oneOf' = foldr (\p acc -> try (string p) <|> acc) parserZero

betweenSpaces :: Parser a -> Parser a
betweenSpaces = between spaces spaces

betweenBrakets :: Parser a -> Parser a
betweenBrakets = between (char '{') (char '}')

