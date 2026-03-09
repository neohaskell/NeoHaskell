module NeoQL.Types (
  FieldName (..),
  Value (..),
  Expr (..),
  FilterResult (..),
) where

import Basics
import Data.Scientific (Scientific)
import Text (Text)


-- | A field name in a NeoQL expression (e.g. "status" from ".status").
newtype FieldName = FieldName Text
  deriving (Eq, Show)


-- | A literal value in a NeoQL equality expression.
data Value
  = StringValue Text
  | NumberValue Scientific
  deriving (Eq, Show)


-- | A NeoQL expression (MVP grammar only).
--
-- FieldAccess: bare field projection (.fieldName) — returns True for all objects
-- FieldEquals: equality filter (.fieldName == value)
data Expr
  = FieldAccess FieldName
  | FieldEquals FieldName Value
  deriving (Eq, Show)


-- | Result of executing a NeoQL expression against a single JSON value.
data FilterResult
  = Matches
  | NoMatch
  | FieldNotFound
  deriving (Eq, Show)
