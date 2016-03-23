{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------
-- |
-- Module: Main
--
--
-- Tests: properties and specs.
--
----------------------------------------------------------------------

module Main
  ( main
  )
  where

-- bookshelf
import Bookshelf (Book(..))

-- aeson
import Data.Aeson
  ( decode
  , encode
  )

-- hspec
import Test.Hspec
  ( describe
  , hspec
  , it
  , shouldBe
  )

-- QuickCheck
import Test.QuickCheck
  ( Arbitrary(arbitrary)
  , Gen
  , generate
  , quickCheck
  )


-- |
--
-- Run specs and properties.

main :: IO ()
main = do
  specs
  props


-- |
--
-- Run specs.

specs :: IO ()
specs =
  hspec $
    describe "decodes/encodes books from/to json" $ do
      let
        book =
          Book
            { bookAuthor = "Charles Dickens"
            , bookTitle = "David Copperfield"
            }

        bookObject =
          "{\"author\":\"Charles Dickens\",\"title\":\"David Copperfield\"}"

      it "there" $
        encode book `shouldBe` bookObject

      it "and back again" $
        decode bookObject `shouldBe` Just book

      it "there and back again" $
        decode (encode book) `shouldBe` Just book

      it "there and back again, again" $ do
        arbitraryBook <- generate (arbitrary :: Gen Book)

        decode (encode arbitraryBook) `shouldBe` Just arbitraryBook


-- |
--
-- Run properties.

props :: IO ()
props =
  quickCheck $
    \book -> decode (encode book) == Just (book :: Book)
