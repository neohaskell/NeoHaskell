{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Automatic schema generation for NeoHaskell types.
--
-- This module provides a library-agnostic schema representation that can be
-- converted to OpenAPI, JSON Schema, or other formats.
--
-- Usage:
--
-- @
-- data AddItem = AddItem
--   { cartId :: Uuid
--   , productId :: Text
--   , quantity :: Int
--   }
--   deriving (Generic)
--
-- instance ToSchema AddItem
-- -- Automatically generates schema with field names and types
-- @
module Schema (
  -- * Schema ADT
  Schema (..),
  FieldSchema (..),

  -- * ToSchema Typeclass
  ToSchema (..),

  -- * Generic Schema Derivation
  GToSchema (..),
  GToFieldSchema (..),
  GToSumSchema (..),
) where

import Array (Array)
import Array qualified
import Basics
import GHC.Generics qualified as Generics
import Maybe (Maybe (..))
import Prelude qualified as GhcPrelude
import Text (Text)
import Text qualified
import Uuid (Uuid)


-- | Library-agnostic schema representation.
-- Can be converted to OpenAPI, JSON Schema, CLI args, etc.
data Schema
  = -- | Unit, ()
    SNull
  | -- | Bool
    SBool
  | -- | Int, Integer
    SInt
  | -- | Double, Float
    SNumber
  | -- | Text, String
    SText
  | -- | Array a, [a]
    SArray Schema
  | -- | Maybe a (marks field as non-required)
    SOptional Schema
  | -- | Record types
    SObject (Array FieldSchema)
  | -- | Sum types without fields (data Status = Active | Inactive)
    SEnum (Array Text)
  | -- | Sum types with fields (data Shape = Circle {radius :: Int} | ...)
    SUnion (Array (Text, Schema))
  | -- | Reference for recursive types
    SRef Text
  deriving (Show, Eq)


-- | Schema for a single field in a record type.
data FieldSchema = FieldSchema
  { fieldName :: Text
  , fieldSchema :: Schema
  , fieldRequired :: Bool
  , fieldDescription :: Text
  }
  deriving (Show, Eq)


-- | Typeclass for types that can be converted to a Schema.
--
-- Types with Generic instances get ToSchema for free using the default
-- implementation.
class ToSchema value where
  toSchema :: Schema
  default toSchema ::
    ( Generics.Generic value
    , GToSchema (Generics.Rep value)
    ) =>
    Schema
  toSchema = gToSchema @(Generics.Rep value)


-- | Generic schema derivation for GHC.Generics representations.
class GToSchema (rep :: Type -> Type) where
  gToSchema :: Schema


-- | Generic field schema derivation for record fields.
class GToFieldSchema (rep :: Type -> Type) where
  gToFieldSchema :: Array FieldSchema


-- | Helper typeclass for sum type variants.
-- Returns list of (constructorName, schema) pairs.
class GToSumSchema (rep :: Type -> Type) where
  gToSumSchema :: Array (Text, Schema)


-- -----------------------------------------------------------------------------
-- Primitive Instances
-- -----------------------------------------------------------------------------

instance ToSchema Bool where
  toSchema = SBool


instance ToSchema Int where
  toSchema = SInt


instance ToSchema GhcPrelude.Integer where
  toSchema = SInt


-- NeoHaskell's Float = Prelude.Double, so one instance covers both
instance ToSchema Float where
  toSchema = SNumber


instance ToSchema Text where
  toSchema = SText


instance {-# OVERLAPPING #-} ToSchema GhcPrelude.String where
  toSchema = SText


-- -----------------------------------------------------------------------------
-- Common NeoHaskell Types
-- -----------------------------------------------------------------------------

instance ToSchema Uuid where
  toSchema = SText -- UUIDs serialize as text/strings


-- -----------------------------------------------------------------------------
-- Container Instances
-- -----------------------------------------------------------------------------

instance (ToSchema element) => ToSchema (Array element) where
  toSchema = SArray (toSchema @element)


instance (ToSchema element) => ToSchema [element] where
  toSchema = SArray (toSchema @element)


instance (ToSchema inner) => ToSchema (Maybe inner) where
  toSchema = SOptional (toSchema @inner)


-- -----------------------------------------------------------------------------
-- Generic Instances: Metadata Wrappers
-- -----------------------------------------------------------------------------

-- | Datatype metadata (D1)
instance (GToSchema rep) => GToSchema (Generics.D1 meta rep) where
  gToSchema = gToSchema @rep


-- | Constructor metadata (C1) - delegates to field handling
instance (GToFieldSchema rep, Generics.Constructor meta) => GToSchema (Generics.C1 meta rep) where
  gToSchema = do
    let fields = gToFieldSchema @rep
    SObject fields


-- | Unit constructor (no fields)
instance GToSchema Generics.U1 where
  gToSchema = SNull


-- -----------------------------------------------------------------------------
-- Generic Instances: Sum Types
-- -----------------------------------------------------------------------------

-- | Sum type (:+:) - uses GToSumSchema to collect variants
instance
  ( GToSumSchema left
  , GToSumSchema right
  ) =>
  GToSchema (left Generics.:+: right)
  where
  gToSchema = do
    let leftVariants = gToSumSchema @left
    let rightVariants = gToSumSchema @right
    let allVariants = Array.append leftVariants rightVariants
    -- Check if all variants are nullary (no fields) -> SEnum
    -- Otherwise -> SUnion
    let isNullary = \(_, schema) ->
          case schema of
            SNull -> True
            _ -> False
    case Array.any isNullary allVariants of
      True ->
        -- If all are nullary, extract just the names as enum
        let names = allVariants |> Array.map (\(name, _) -> name)
         in case Array.any (not <. isNullary) allVariants of
              True -> SUnion allVariants -- Mixed: some have fields
              False -> SEnum names -- All nullary
      False ->
        SUnion allVariants


-- | Sum type variant (:+:) - combine variants from left and right
instance
  ( GToSumSchema left
  , GToSumSchema right
  ) =>
  GToSumSchema (left Generics.:+: right)
  where
  gToSumSchema = do
    let leftVariants = gToSumSchema @left
    let rightVariants = gToSumSchema @right
    Array.append leftVariants rightVariants


-- | Constructor in sum type - extract name and schema
instance
  ( Generics.Constructor meta
  , GToFieldSchema rep
  ) =>
  GToSumSchema (Generics.C1 meta rep)
  where
  gToSumSchema = do
    let constructorName = Generics.conName (GhcPrelude.undefined :: Generics.C1 meta rep p)
    let fields = gToFieldSchema @rep
    let schema = case Array.isEmpty fields of
          True -> SNull -- Nullary constructor
          False -> SObject fields
    Array.fromLinkedList [(Text.fromLinkedList constructorName, schema)]


-- -----------------------------------------------------------------------------
-- Generic Instances: Field Schema
-- -----------------------------------------------------------------------------

-- | Empty/unit produces no fields
instance GToFieldSchema Generics.U1 where
  gToFieldSchema = Array.empty


-- | Product type (:*:) - combine fields from left and right
instance
  ( GToFieldSchema left
  , GToFieldSchema right
  ) =>
  GToFieldSchema (left Generics.:*: right)
  where
  gToFieldSchema = do
    let leftFields = gToFieldSchema @left
    let rightFields = gToFieldSchema @right
    Array.append leftFields rightFields


-- | Constructor wrapper - delegates to inner
instance (GToFieldSchema rep) => GToFieldSchema (Generics.C1 meta rep) where
  gToFieldSchema = gToFieldSchema @rep


-- | Field with selector name (S1) - extracts field name and schema
instance
  {-# OVERLAPPABLE #-}
  ( Generics.Selector selector
  , ToSchema fieldType
  ) =>
  GToFieldSchema (Generics.S1 selector (Generics.K1 index fieldType))
  where
  gToFieldSchema = do
    let name = Generics.selName (GhcPrelude.undefined :: Generics.S1 selector (Generics.K1 index fieldType) p)
    let schema = toSchema @fieldType
    let required = case schema of
          SOptional _ -> False
          _ -> True
    Array.fromLinkedList
      [ FieldSchema
          { fieldName = Text.fromLinkedList name
          , fieldSchema = schema
          , fieldRequired = required
          , fieldDescription = ""
          }
      ]


-- | Special case: Maybe fields are non-required
instance
  {-# OVERLAPPING #-}
  ( Generics.Selector selector
  , ToSchema inner
  ) =>
  GToFieldSchema (Generics.S1 selector (Generics.K1 index (Maybe inner)))
  where
  gToFieldSchema = do
    let name = Generics.selName (GhcPrelude.undefined :: Generics.S1 selector (Generics.K1 index (Maybe inner)) p)
    let innerSchema = toSchema @inner
    Array.fromLinkedList
      [ FieldSchema
          { fieldName = Text.fromLinkedList name
          , fieldSchema = SOptional innerSchema
          , fieldRequired = False
          , fieldDescription = ""
          }
      ]
