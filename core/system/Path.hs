module Path (
  Path,
  fromText,
  toLinkedList,
  toText,
  path,
) where

-- Import the reexported types and functions

-- Import qualified for using internally and renaming

import Basics
import Char (Char)
import Control.Monad.Fail qualified as Monad
import Json qualified
import Language.Haskell.TH.Quote qualified as Quote
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax qualified as TH
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import Text (Text)
import Text qualified
import ToText qualified


newtype Path = Path (LinkedList Char) -- We use LinkedList Char to keep compatibility with Haskell's FilePath type
  deriving (Lift, ToText.Show, Eq, Ord, Json.FromJSON, Json.ToJSON)


fromText :: Text -> Maybe Path
fromText text =
  -- FIXME: Implement proper path parsing by using
  -- a filepath parsing library
  Just (Path (Text.toLinkedList text))


-- | Smart text constructor to make a path from a text literal
path :: Quote.QuasiQuoter
path =
  Quote.QuasiQuoter
    { Quote.quoteExp = \text -> do
        case fromText (Text.fromLinkedList text) of
          Just p -> TH.lift p
          Nothing -> Monad.fail "Invalid path",
      Quote.quotePat = dieWith "path constructor can only be used in expressions",
      Quote.quoteType = dieWith "path constructor can only be used in expressions",
      Quote.quoteDec = dieWith "path constructor can only be used in expressions"
    }


toText :: Path -> Text
toText (Path linkedList) =
  Text.fromLinkedList linkedList


toLinkedList :: Path -> LinkedList Char
toLinkedList (Path linkedList) =
  linkedList