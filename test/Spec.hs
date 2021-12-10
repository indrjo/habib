{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Test.Hspec
import Test.Hspec.Attoparsec
import Habib

main :: IO ()
main = hspec spec

demoSpec :: Spec
demoSpec = do
  describe "shouldParse" $
    it "works on: \"x\" ~> char 'x'" $
      ("x" :: Text) ~> char 'x'
        `shouldParse` 'x'

  describe "parseSatisfies" $ do
    it "works on: \"x\" and (=='x')" $
      ("x" :: Text) ~> char 'x'
        `parseSatisfies` (=='x')

    it "\">>>\" satisfies length == 3 when parser as a list of char" $
      (">>>" :: Text) ~> many (char '>')
        `parseSatisfies` ((==3) . Prelude.length)

  describe "shouldFailOn" $
    it "char 'x' fails on \"ha\"" $
      char 'x' `shouldFailOn` ("ha" :: Text)

  describe "shouldSucceedOn" $
    it "char 'x' succeeds on \"x\"" $
      char 'x' `shouldSucceedOn` ("x" :: Text)

  describe "leavesUnconsumed" $ do
    it "works on \"xa\" ~?> char 'x'" $
      ("xa" :: Text) ~?> char 'x'
        `leavesUnconsumed` "a"

    it "char 'x' leaves nothing unconsumed on \"x\"" $
      ("x" :: Text) ~?> char 'x'
        `leavesUnconsumed` ""

spec :: Spec
spec = do
  describe "bibType" $ do
    it "works on: `article`" $ ("@article" :: Text) ~> entryType `shouldParse` "article"
    it "works on: `book`" $ ("@book" :: Text) ~> entryType `shouldParse` "book"
    it "works on: `booklet`" $ ("@booklet" :: Text) ~> entryType `shouldParse` "booklet"
    it "works on: `conference`" $ ("@conference" :: Text) ~> entryType `shouldParse` "conference"
    it "works on: `inbook`" $ ("@inbook" :: Text) ~> entryType `shouldParse` "inbook"
    it "works on: `incollection`" $ ("@incollection" :: Text) ~> entryType `shouldParse` "incollection"
    it "works on: `inproceedings`" $ ("@inproceedings" :: Text) ~> entryType `shouldParse` "inproceedings"
    it "works on: `manual`" $ ("@manual" :: Text) ~> entryType `shouldParse` "manual"
    it "works on: `mastersthesis`" $ ("@mastersthesis" :: Text) ~> entryType `shouldParse` "mastersthesis"
    it "works on: `misc`" $ ("@misc" :: Text) ~> entryType `shouldParse` "misc"
    it "works on: `phdthesis`" $ ("@phdthesis" :: Text) ~> entryType `shouldParse` "phdthesis"
    it "works on: `proceedings`" $ ("@proceedings" :: Text) ~> entryType `shouldParse` "proceedings"
    it "works on: `techreport`" $ ("@techreport" :: Text) ~> entryType `shouldParse` "techreport"
    it "works on: `unpublished`" $ ("@unpublished" :: Text) ~> entryType `shouldParse` "unpublished"
    it "doesn't works on: `pappero`" $ entryType `shouldFailOn` ("@pappero" :: Text)

  describe "bibKey" $ do
    it "works on `kelly1972abstract`" $ ("kelly1972abstract," :: Text)  ~> bibKey `shouldParse` "kelly1972abstract"
    it "doesn't works on: `kelly{1972abstract`" $ ("kelly{1972abstract" :: Text) ~?> bibKey `leavesUnconsumed` "{1972abstract"
    it "doesn't works on: `kelly197#2abstract`" $ ("kelly197#2abstract" :: Text) ~?> bibKey `leavesUnconsumed` "#2abstract"
    it "doesn't works on: `kelly1972%abstract`" $ ("kelly1972%abstract" :: Text) ~?> bibKey `leavesUnconsumed` "%abstract"
    it "doesn't works on: `kelly19}72abstract`" $ ("kelly19}72abstract" :: Text) ~?> bibKey `leavesUnconsumed` "}72abstract"
    it "doesn't works on: `kelly1972a,bstract`" $ ("kelly1972a,bstract" :: Text) ~?> bibKey `leavesUnconsumed` ",bstract"
    it "doesn't works on: `k~elly1972abstract`" $ ("k~elly1972abstract" :: Text) ~?> bibKey `leavesUnconsumed` "~elly1972abstract"
    it "doesn't works on: `kel ly1972abstract`" $ ("kel ly1972abstract" :: Text) ~?> bibKey `leavesUnconsumed` " ly1972abstract"
    it "doesn't works on: `kelly1972\\abstract`" $ ("kelly1972\\abstract" :: Text) ~?> bibKey `leavesUnconsumed` "\\abstract"