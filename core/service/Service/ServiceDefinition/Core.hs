{- HLINT ignore "Use camelCase" -}
module Service.ServiceDefinition.Core (
  Service,
  new,
  useServer,
  command,
  __internal_runServiceMain,
) where

import Basics
import GHC.IO qualified as GHC


data Service = Service


new :: Service
new = Service


useServer ::
  sv -> sd -> sd2
useServer _ _ =
  panic "use server not implemented"


-- | Register a command type in the service definition
command ::
  sd ->
  sd2
command =
  panic "command not implemented"


__internal_runServiceMain ::
  a -> GHC.IO Unit
__internal_runServiceMain _ =
  panic "__internal_runServiceMain - not implemented"