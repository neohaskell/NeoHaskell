{-# LANGUAGE ExistentialQuantification #-}

-- | Core types for the Config DSL.
--
-- This module defines the data types used to represent configuration
-- field definitions before they are compiled into opt-env-conf parsers.
module Config.Core (
  -- * Field Definition
  FieldDef (..),
  FieldModifier (..),

  -- * Validation
  ConfigError (..),
  validateFieldDef,
) where

import Core
import Data.List qualified as GhcList
import Language.Haskell.TH.Syntax qualified as TH


-- | Modifiers that can be applied to a config field.
--
-- These are accumulated by builder functions and converted to
-- opt-env-conf builders during TH expansion.
data FieldModifier
  = -- | Documentation string (shown in --help and error messages)
    ModDoc Text
  | -- | Environment variable name override
    ModEnvVar Text
  | -- | Default value (as a TH Q Exp for splicing)
    ModDefault (TH.Q TH.Exp)
  | -- | Field is required (mutually exclusive with ModDefault)
    ModRequired
  | -- | Sensitive value - redacted in logs and --help
    ModSecret
  | -- | Long CLI option name (e.g., "port" for --port)
    ModCliLong Text
  | -- | Short CLI option character (e.g., 'p' for -p)
    ModCliShort Char
  | -- | Environment variable prefix for nested configs
    ModEnvPrefix Text


-- | A single field definition within a config type.
--
-- Built incrementally using the Config.Builder combinators:
--
-- @
-- Config.field \@Int "port"
--   |> Config.doc "HTTP port to listen on"
--   |> Config.defaultsTo 8080
-- @
data FieldDef = FieldDef
  { fieldName :: Text
  -- ^ Field name (becomes the record field name)
  , fieldType :: TH.Type
  -- ^ The Haskell type of this field
  , fieldModifiers :: [FieldModifier]
  -- ^ Accumulated modifiers
  }


-- | Validation errors detected at compile time.
data ConfigError
  = -- | Field is missing Config.doc
    MissingDoc Text
  | -- | Field has neither defaultsTo nor required
    MissingDefaultOrRequired Text
  | -- | Field has both defaultsTo and required (invalid)
    BothDefaultAndRequired Text
  deriving (Show, Eq)


-- | Validate a field definition, returning any errors found.
validateFieldDef :: FieldDef -> [ConfigError]
validateFieldDef fd = do
  let name = fd.fieldName
  let mods = fd.fieldModifiers
  let hasDoc' = hasModifier isDoc mods
  let hasDefault' = hasModifier isDefault mods
  let hasRequired' = hasModifier isRequired mods
  let errors = []
        |> addIf (not hasDoc') (MissingDoc name)
        |> addIf (not hasDefault' && not hasRequired') (MissingDefaultOrRequired name)
        |> addIf (hasDefault' && hasRequired') (BothDefaultAndRequired name)
  errors


-- Internal helpers

hasModifier :: (FieldModifier -> Bool) -> [FieldModifier] -> Bool
hasModifier predicate mods =
  GhcList.any predicate mods


isDoc :: FieldModifier -> Bool
isDoc modifier =
  case modifier of
    ModDoc _ -> True
    _ -> False


isDefault :: FieldModifier -> Bool
isDefault modifier =
  case modifier of
    ModDefault _ -> True
    _ -> False


isRequired :: FieldModifier -> Bool
isRequired modifier =
  case modifier of
    ModRequired -> True
    _ -> False


addIf :: Bool -> item -> [item] -> [item]
addIf condition item arr =
  case condition of
    True -> arr ++ [item]
    False -> arr
