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
  enum,
  nested,

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
-- ModNested is used in the nested function
import Core
import Data.Typeable qualified as Typeable
import Language.Haskell.TH.Syntax qualified as TH
import LinkedList qualified


-- | Start defining a configuration field with the given type and name.
--
-- The type is captured via TypeApplications:
--
-- @
-- Config.field \@Int "port"
-- Config.field \@Text "databaseUrl"
-- Config.field \@(Secret Text) "apiKey"
-- Config.field \@(Maybe Int) "optionalPort"
-- @
field :: forall fieldType. (Typeable.Typeable fieldType) => Text -> FieldDef
field name = do
  let typeRep = Typeable.typeRep (Typeable.Proxy @fieldType)
  let thType = typeRepToTHType typeRep
  FieldDef
    { fieldName = name
    , fieldType = thType
    , fieldModifiers = []
    }


-- | Convert a Typeable TypeRep to a Template Haskell Type.
--
-- This properly handles:
-- - Simple types: Int, Text, Bool
-- - Applied types: Maybe Int, Array Text, Either String Int
-- - Nested applications: Maybe (Array Text)
-- - External package types with proper qualification
typeRepToTHType :: Typeable.TypeRep -> TH.Type
typeRepToTHType typeRep = do
  let tyCon = Typeable.typeRepTyCon typeRep
  let args = Typeable.typeRepArgs typeRep
  let baseName = tyConToTHName tyCon
  let baseType = TH.ConT baseName
  -- Fold arguments with AppT: Maybe Int -> AppT (ConT Maybe) (ConT Int)
  let argTypes = LinkedList.map typeRepToTHType args
  LinkedList.foldl (\arg acc -> TH.AppT acc arg) baseType argTypes


-- | Convert a TyCon to a fully-qualified TH Name using mkNameG_tc.
--
-- This ensures proper name resolution across package boundaries.
tyConToTHName :: Typeable.TyCon -> TH.Name
tyConToTHName tyCon = do
  let pkg = Typeable.tyConPackage tyCon
  let modName = Typeable.tyConModule tyCon
  let tyName = Typeable.tyConName tyCon
  TH.mkNameG_tc pkg modName tyName


-- | Define an enum configuration field.
--
-- This is semantically equivalent to 'field' but makes the intent clear
-- when defining enum-typed configuration. The enum type must have 'Read'
-- and 'Show' instances for parsing from strings.
--
-- @
-- data LogLevel = Debug | Info | Warn | Error
--   deriving (Show, Read, Generic)
--
-- Config.enum \@LogLevel "logLevel"
--   |> Config.doc "Minimum log level"
--   |> Config.defaultsTo Info
-- @
enum :: forall enumType. (Typeable.Typeable enumType) => Text -> FieldDef
enum = field @enumType


-- | Define a nested configuration field.
--
-- Nested configs allow composing multiple config types together. The nested
-- type must also have a 'HasParser' instance (typically from its own
-- 'defineConfig' declaration).
--
-- Use 'envPrefix' to namespace the nested config's environment variables.
--
-- @
-- defineConfig "DatabaseConfig"
--   [ Config.field \@Text "host"
--       |> Config.doc "Database host"
--       |> Config.defaultsTo "localhost"
--   , Config.field \@Int "port"
--       |> Config.doc "Database port"
--       |> Config.defaultsTo 5432
--   ]
--
-- defineConfig "AppConfig"
--   [ Config.nested \@DatabaseConfig "database"
--       |> Config.doc "Database connection settings"
--       |> Config.envPrefix "DB_"
--   ]
-- @
--
-- With @envPrefix "DB_"@, the nested fields become:
--
-- * @DB_HOST@ (defaults to "localhost")
-- * @DB_PORT@ (defaults to 5432)
nested :: forall nestedType. (Typeable.Typeable nestedType) => Text -> FieldDef
nested name = do
  let typeRep = Typeable.typeRep (Typeable.Proxy @nestedType)
  let thType = typeRepToTHType typeRep
  FieldDef
    { fieldName = name
    , fieldType = thType
    , fieldModifiers = [ModNested]
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
