{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Service.Protocol (
  -- * Server API
  ServerApi(..),
  ApiFor,
) where

import Basics

-- | Type class for server APIs
-- Each API specifies its configuration and runtime state types
-- This is primarily a marker class for type-level API tracking
class ServerApi (apiName :: Symbol) where
  -- | Configuration type for this API
  type ApiConfig apiName :: Type

  -- | Runtime state type for this API
  type ApiState apiName :: Type

-- | Type family to get the list of APIs required by a command
-- Each command type declares which APIs it needs to be exposed through
-- For example: type instance ApiFor CreateCart = '["WebApi", "GrpcApi"]
type family ApiFor (commandType :: Type) :: [Symbol]