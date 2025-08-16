module File (
  Error (..),
  readText,
  writeText,
  static,
) where

import Basics
import Control.Monad.Fail qualified as GHC
import Data.FileEmbed qualified as FileEmbed
import Data.Text.IO qualified as TIO
import GHC.IO.Exception qualified as Exception
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as Quote
import Path (Path)
import Path qualified
import Task (Task)
import Task qualified
import Text (Text)
import ToText (Show (..))


data Error
  = NotFound
  | NotWritable
  | NotReadable
  deriving (Show)


readText :: Path -> Task Error Text
readText filepath =
  filepath
    |> Path.toLinkedList
    |> TIO.readFile
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotReadable)


writeText :: Path -> Text -> Task Error ()
writeText path textToWrite =
  TIO.writeFile (Path.toLinkedList path) textToWrite
    |> Task.fromFailableIO @Exception.IOError
    |> Task.mapError (\_ -> NotWritable)


static :: Quote.QuasiQuoter
static =
  Quote.QuasiQuoter
    { Quote.quoteExp = \filePath ->
        FileEmbed.embedFile filePath,
      Quote.quotePat = \_ -> GHC.fail "File.static cannot be used in patterns",
      Quote.quoteType = \_ -> GHC.fail "File.static cannot be used in types",
      Quote.quoteDec = \_ -> GHC.fail "File.static cannot be used in declarations"
    }
