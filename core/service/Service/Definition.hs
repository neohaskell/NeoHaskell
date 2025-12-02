{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Service.Definition (
  -- * Service Definition
  ServiceDefinition(..),

  -- * Type families
  module Service.Definition.TypeLevel,
  module Service.Definition.Validation,
) where

import Basics hiding (fmap, join, pure, return, (<*>), (>>), (>>=))
import Record (Record)
import Record qualified
import Service.Definition.TypeLevel
import Service.Definition.Validation

-- | ServiceDefinition represents a service with transport protocol tracking
-- It accumulates commands, required protocols, provided protocols, and adapters
-- The type parameters track this information at the type level
data ServiceDefinition
  (commands :: Record.Row Type)           -- Row type of all registered commands
  (requiredProtocols :: [Symbol])         -- Type-level list of protocols needed by commands
  (providedProtocols :: [Symbol])         -- Type-level list of protocols with adapters
  (adapters :: Record.Row Type)           -- Row type of adapter instances
  (value :: Type)                         -- Monadic value parameter
  = ServiceDefinition
  { commandRecord :: Record commands,
    adapterRecord :: Record adapters,
    value :: value
  }
  deriving (Generic)