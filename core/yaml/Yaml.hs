module Yaml (
  parse,
  ParseError,
) where

import Bytes qualified
import Core
import Data.Either qualified as Either
import Data.Yaml qualified
import Json qualified
import Result qualified
import Text qualified


data ParseError
  = ParseError Text


parse :: (Json.Decodable value) => Text -> Result ParseError value
parse text = do
  let (Bytes.INTERNAL_CORE_BYTES_CONSTRUCTOR bytes) = Text.toBytes text
  case Data.Yaml.decodeEither' bytes of
    Either.Left error -> Result.Err (ParseError (toText error))
    Either.Right value -> Result.Ok value