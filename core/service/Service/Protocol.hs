{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Service.Protocol (
  -- * Transport Protocol
  TransportProtocol(..),
  TransportProtocols,
) where

import Basics

-- | Type class for transport protocols
-- Each protocol specifies its configuration and runtime state types
-- This is primarily a marker class for type-level protocol tracking
class TransportProtocol (protocolName :: Symbol) where
  -- | Configuration type for this protocol
  type ProtocolConfig protocolName :: Type

  -- | Runtime state type for this protocol
  type ProtocolState protocolName :: Type

-- | Type family to get the list of transport protocols for a command
-- Each command type can declare which protocols it supports
type family TransportProtocols (commandType :: Type) :: [Symbol]