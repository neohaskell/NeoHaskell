{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Builder combinators for the Config DSL.
--
-- These functions provide a fluent API for defining configuration fields:
--
-- @
-- Config.field \@Int "port"
--   |> Config.doc "HTTP port to listen on"
--   |> Config.defaultsTo 8080
--   |> Config.envVar "PORT"
--   |> Config.cliLong "port"
--   |> Config.cliShort 'p'
-- @
module Config.Builder (
  -- * Field Definition
  field,

  -- * Modifiers
  doc,
  envVar,
  defaultsTo,
  required,
  secret,
  cliLong,
  cliShort,
  envPrefix,
) where

import Config.Core (FieldDef (..), FieldModifier (..))
import Core
import Data.Typeable qualified as Typeable
import Language.Haskell.TH.Syntax qualified as TH


-- | Start defining a configuration field with the given type and name.
--
-- The type is captured via TypeApplications:
--
-- @
-- Config.field \@Int "port"
-- Config.field \@Text "databaseUrl"
-- Config.field \@(Secret Text) "apiKey"
-- @
field :: forall fieldType. (Typeable.Typeable fieldType) => Text -> FieldDef
field name = do
  let typeRep = Typeable.typeRep (Typeable.Proxy @fieldType)
  let typeName = Typeable.showsTypeRep typeRep ""
  FieldDef
    { fieldName = name
    , fieldType = TH.ConT (TH.mkName typeName)
    , fieldModifiers = []
    }


-- | Add documentation for a field.
--
-- Documentation is REQUIRED and will cause a compile-time error if missing.
-- It appears in @--help@ output and error messages.
--
-- @
-- Config.field \@Int "port"
--   |> Config.doc "HTTP port to listen on"
-- @
doc :: Text -> FieldDef -> FieldDef
doc description fd =
  fd {fieldModifiers = ModDoc description : fd.fieldModifiers}


-- | Specify the environment variable name for a field.
--
-- If not specified, the field name is converted to SCREAMING_SNAKE_CASE:
-- @databaseUrl@ becomes @DATABASE_URL@.
--
-- @
-- Config.field \@Text "databaseUrl"
--   |> Config.envVar "DATABASE_URL"
-- @
envVar :: Text -> FieldDef -> FieldDef
envVar varName fd =
  fd {fieldModifiers = ModEnvVar varName : fd.fieldModifiers}


-- | Set a default value for a field.
--
-- Mutually exclusive with 'required'. Using both causes a compile-time error.
--
-- The value must have a Lift instance so it can be spliced into the generated code.
--
-- @
-- Config.field \@Int "port"
--   |> Config.defaultsTo 8080
-- @
defaultsTo :: forall value. (TH.Lift value) => value -> FieldDef -> FieldDef
defaultsTo val fd =
  fd {fieldModifiers = ModDefault (TH.lift val) : fd.fieldModifiers}


-- | Mark a field as required.
--
-- Required fields must be provided via CLI, environment variable, or config file.
-- Missing required fields cause startup to fail with a helpful error message.
--
-- Mutually exclusive with 'defaultsTo'. Using both causes a compile-time error.
--
-- @
-- Config.field \@Text "databaseUrl"
--   |> Config.required
-- @
required :: FieldDef -> FieldDef
required fd =
  fd {fieldModifiers = ModRequired : fd.fieldModifiers}


-- | Mark a field as containing sensitive data.
--
-- Secret fields:
-- - Are redacted in @--help@ output
-- - Are redacted in error messages
-- - Should use the @Secret@ wrapper type for additional protection
--
-- @
-- Config.field \@(Secret Text) "apiKey"
--   |> Config.secret
--   |> Config.required
-- @
secret :: FieldDef -> FieldDef
secret fd =
  fd {fieldModifiers = ModSecret : fd.fieldModifiers}


-- | Add a long CLI option flag.
--
-- @
-- Config.field \@Int "port"
--   |> Config.cliLong "port"  -- enables --port 8080
-- @
cliLong :: Text -> FieldDef -> FieldDef
cliLong name fd =
  fd {fieldModifiers = ModCliLong name : fd.fieldModifiers}


-- | Add a short CLI option flag.
--
-- @
-- Config.field \@Int "port"
--   |> Config.cliShort 'p'  -- enables -p 8080
-- @
cliShort :: Char -> FieldDef -> FieldDef
cliShort char fd =
  fd {fieldModifiers = ModCliShort char : fd.fieldModifiers}


-- | Set environment variable prefix for nested configs.
--
-- When using nested configs, this prefix is prepended to all nested field
-- environment variable names.
--
-- @
-- Config.field \@DatabaseConfig "database"
--   |> Config.envPrefix "DB_"
-- -- Nested fields: DB_HOST, DB_PORT, DB_NAME, etc.
-- @
envPrefix :: Text -> FieldDef -> FieldDef
envPrefix prefix fd =
  fd {fieldModifiers = ModEnvPrefix prefix : fd.fieldModifiers}
