-- | NeoQL: A minimal query language for NeoHaskell query endpoints.
--
-- MVP supports field access (.fieldName) and equality filtering (.fieldName == value).
-- See ADR-0040 for design decisions.
module NeoQL (
  -- * Parsing
  parse,
  -- * Execution
  execute,
  filterValues,
  -- * Types
  Expr (..),
  FieldName (..),
  Value (..),
  FilterResult (..),
) where

import NeoQL.Execute (execute, filterValues)
import NeoQL.Parser (parse)
import NeoQL.Types (Expr (..), FieldName (..), FilterResult (..), Value (..))
