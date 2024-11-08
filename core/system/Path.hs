module Path (
  Path,
  fromText,
  toLinkedList,
  toText,
  path,
  joinPaths,
) where

import Array (Array)
import Array qualified
import Basics
import Char (Char)
import Control.Monad.Fail qualified as Monad
import Json qualified
import Language.Haskell.TH.Quote qualified as Quote
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax qualified as TH
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import Maybe qualified
import Text (Text)
import Text qualified
import ToText qualified


newtype Path = Path (LinkedList Char) -- We use LinkedList Char to keep compatibility with Haskell's FilePath type
  deriving (Lift, Eq, Ord, Json.FromJSON, Json.ToJSON)


instance ToText.Show Path where
  show (Path p) = p


instance IsString Path where
  fromString = Path


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
      Quote.quotePat = panic "path constructor can only be used in expressions",
      Quote.quoteType = panic "path constructor can only be used in expressions",
      Quote.quoteDec = panic "path constructor can only be used in expressions"
    }


toText :: Path -> Text
toText (Path linkedList) =
  Text.fromLinkedList linkedList


toLinkedList :: Path -> LinkedList Char
toLinkedList (Path linkedList) =
  linkedList


-- | Joins paths in a cross-platform way
-- TODO: Make this cross platform lol
joinPaths :: Array Path -> Path
joinPaths paths =
  paths
    |> Array.map toText
    |> Text.joinWith "/"
    |> fromText
    |> Maybe.getOrDie