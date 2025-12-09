{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Service.Protocol (
  -- * Server API
  ServerApi (..),
  ApiFor,
) where

import Basics
import Task (Task)
import Text (Text)


-- | Type class for server APIs
-- Each API type can specify configuration and runtime state
-- This is primarily a marker class for type-level API tracking
class ServerApi (api :: Type) where
  -- | Unique name for this server API (used for storage keying)
  type ServerName api :: Symbol


  type ApiState api :: Type
  type ApiState api = Unit


  type ApiError api :: Type
  type ApiError api = Text


  runServer :: api -> Task (ApiError api) Unit


-- | Type family to get the list of API types required by a command
-- Each command type declares which API types it needs to be exposed through
-- For example: type instance ApiFor CreateCart = '[WebApi, GrpcApi]
-- Where WebApi and GrpcApi are types that carry configuration
type family ApiFor (commandType :: Type) :: [Type]