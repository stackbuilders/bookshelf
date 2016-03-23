{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Bookshelf
--
--
-- A bookshelf.
--
-- Actually, books.
--
----------------------------------------------------------------------

module Bookshelf
  ( Book(..)
  )
  where

-- aeson
import Data.Aeson
  ( (.:)
  , (.=)
  , FromJSON(parseJSON)
  , object
  , ToJSON(toJSON)
  , withObject
  )

-- QuickCheck
import Test.QuickCheck
  ( Arbitrary(arbitrary)
  , elements
  )


-- |
--
-- A book.

data Book =
  Book
    { bookAuthor :: String -- ^ The author.
    , bookTitle :: String -- ^ The title.
    }
  deriving Eq


-- |
--
-- Show a book.

instance Show Book where
  show Book{..} =
    bookTitle ++ " (" ++ bookAuthor ++ ")"


-- |
--
-- Parse a book from JSON.

instance FromJSON Book where
  parseJSON =
    withObject "Book Object" $
      \ob ->
        Book
          <$> ob .: "author"
          <*> ob .: "title"


-- |
--
-- Convert a book to JSON.

instance ToJSON Book where
  toJSON Book{..} =
    object
      [ "author" .= bookAuthor
      , "title" .= bookTitle
      ]


-- |
--
-- Generate a book.

instance Arbitrary Book where
  arbitrary = do
    author <-
      elements
        [ "Charles Dickens"
        , "Gabriel García Márquez"
        , "Harper Lee"
        , "José Saramago"
        , "Lewis Carroll"
        , "Roald Dahl"
        ]
    title <-
      elements
        [ "A Christmas Carol"
        , "A Tale of Two Cities"
        , "Alice's Adventures in Wonderland"
        , "Charlie and the Chocolate Factory"
        , "David Copperfield"
        , "Fantastic Mr Fox"
        , "George's Marvellous Medicine"
        , "Go Set a Watchman"
        , "Great Expectations"
        , "James and the Giant Peach"
        , "Matilda"
        , "Oliver Twist"
        , "The BFG"
        , "The Old Curiosity Shop"
        , "The Twits"
        , "The Witches"
        , "Through the Looking-Glass"
        , "To Kill a Mockingbird"
        ]
    return
      Book
        { bookAuthor = author
        , bookTitle = title
        }
