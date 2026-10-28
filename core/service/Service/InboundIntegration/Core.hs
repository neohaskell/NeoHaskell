{-# LANGUAGE AllowAmbiguousTypes #-}

-- | # Service.InboundIntegration.Core
--
-- A typed inbound handler identifies its complete command and target-entity
-- output set. Use 'inboundIntegration' to derive an instance from a worker.
module Service.InboundIntegration.Core (
  InboundIntegration (..),
) where

import Basics
import Integration qualified


-- | A handler whose worker emits only the listed command and entity pairs.
class InboundIntegration handler where
  type InboundConfigOf handler :: Type
  type InboundOutputsOf handler :: [(Type, Type)]
  workerImplFor :: InboundConfigOf handler -> Integration.TypedInbound (InboundOutputsOf handler)
