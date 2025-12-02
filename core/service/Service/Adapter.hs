{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Service.Adapter (
  -- * Service Adapter
  ServiceAdapter(..),
) where

import Basics
import Bytes (Bytes)
import Service.Error (ServiceError)
import Task (Task)

-- | Type class for service adapters
-- Defines how an adapter integrates with the service
class ServiceAdapter (adapter :: Type) where
  -- | The protocol this adapter implements
  type AdapterProtocol adapter :: Symbol

  -- | Initialize the adapter with its configuration
  initializeAdapter :: adapter -> Task ServiceError (AdapterState adapter)

  -- | Execute a command through this adapter
  -- Receives Bytes input and produces Bytes output
  executeCommand ::
    forall commandName.
    adapter ->
    AdapterState adapter ->
    commandName ->
    Bytes ->
    Task ServiceError Bytes

  -- | Gracefully shutdown the adapter
  shutdownAdapter :: adapter -> AdapterState adapter -> Task ServiceError Unit

  -- | Runtime state type for this adapter
  type AdapterState adapter :: Type