module NeoQL.Execute (
  execute,
  filterValues,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Maybe (Maybe (..))
import NeoQL.Types (Expr (..), FieldName (..), FilterResult (..), Value (..))


-- | Execute a NeoQL expression against a single Aeson.Value.
--
-- Returns True if the value matches the expression, False otherwise.
-- For FieldAccess: always returns True (projection, no filtering).
-- For FieldEquals: looks up the field and compares.
-- Non-object values return False.
-- Unknown fields return False (silent non-match).
execute :: Expr -> Aeson.Value -> Bool
execute expr value = do
  case matchesExpr expr value of
    Matches -> True
    NoMatch -> False
    FieldNotFound -> False


-- | Filter an array of Aeson.Value, keeping only those that match the expression.
filterValues :: Expr -> Array Aeson.Value -> Array Aeson.Value
filterValues expr values =
  values |> Array.takeIf (execute expr)


-- | Internal: match an expression against a value, returning a FilterResult.
matchesExpr :: Expr -> Aeson.Value -> FilterResult
matchesExpr expr value =
  case expr of
    FieldAccess _ ->
      -- Bare field access is a projection — no filtering, always matches
      Matches
    FieldEquals (FieldName fieldName) expectedValue ->
      case value of
        Aeson.Object obj ->
          do
            let key = AesonKey.fromText fieldName
            case AesonKeyMap.lookup key obj of
              Maybe.Nothing ->
                FieldNotFound
              Maybe.Just fieldValue ->
                case compareValues expectedValue fieldValue of
                  True -> Matches
                  False -> NoMatch
        _ ->
          -- Non-object values (arrays, strings, numbers, null, bool) never match
          FieldNotFound


-- | Compare a NeoQL Value against an Aeson.Value.
-- Returns True only on exact type + value match.
compareValues :: Value -> Aeson.Value -> Bool
compareValues expected actual =
  case expected of
    StringValue expectedText ->
      case actual of
        Aeson.String actualText ->
          actualText == expectedText
        _ ->
          False
    NumberValue expectedSci ->
      case actual of
        Aeson.Number actualSci ->
          actualSci == expectedSci
        _ ->
          False
