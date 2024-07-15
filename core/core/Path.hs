module Path (
  Path,
  fromText,
  toLinkedList,
  toText,
) where

-- Import the reexported types and functions

-- Import qualified for using internally and renaming

import Char (Char)
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import Text (Text)
import Text qualified


newtype Path = Path (LinkedList Char) -- We use LinkedList Char to keep compatibility with Haskell's FilePath type


fromText :: Text -> Maybe Path
fromText text =
  -- FIXME: Implement proper path parsing by using
  -- a filepath parsing library
  Just (Path (Text.toLinkedList text))


toText :: Path -> Text
toText (Path linkedList) =
  Text.fromLinkedList linkedList


toLinkedList :: Path -> LinkedList Char
toLinkedList (Path linkedList) =
  linkedList