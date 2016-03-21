module Main
  ( main
  )
  where

-- bookshelf
import Bookshelf

-- aeson
import Data.Aeson

-- hspec
import Test.Hspec

-- QuickCheck
import Test.QuickCheck


-- |
--
-- Run specs and properties.

main :: IO ()
main = do
  specs
  props


-- |
--
-- Specs.

specs :: IO ()
specs =
  hspec $
    describe "parses/converts books from/to json" $ do
      it "there and back again" $ do
        let
          book = Book "Charles Dickens" "David Copperfield"

        decode (encode book) `shouldBe` Just book

      it "there and back again, again" $ do
        book <- generate (arbitrary :: Gen Book)

        decode (encode book) `shouldBe` Just book


-- |
--
-- Properties.

props :: IO ()
props =
  quickCheck $
    \book -> decode (encode book) == Just (book :: Book)
