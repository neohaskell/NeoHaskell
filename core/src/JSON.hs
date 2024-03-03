{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module JSON (decodeSchema) where

-- TODO: Cleanup Lazy imports into its own data types

import Core (todo)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonType
import Traits.Schema qualified as Schema


decodeSchema :: Schema.SchemaDefinition -> Aeson.Value -> AesonType.Parser a
decodeSchema = todo
