-- | Type-erased registry of pre-bound dispatcher closures, keyed by
-- 'TypeRep'.
--
-- Populated once at startup by 'Application.run', read once per @emit@ at
-- request time.
module Service.Integration.DispatchRegistry
  ( DispatchRegistry,
    empty,
    register,
    lookup,
    size,
  )
where

import Basics
import Data.Map.Strict qualified as GhcMap
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import Maybe (Maybe (..))
import Service.Integration.Adapter (Integration, Response)
import Service.Integration.IntegrationError (IntegrationError)
import Task (Task)
import Unknown (Unknown)
import Unknown qualified


-- | Map of 'TypeRep' to a type-erased dispatcher closure.
newtype DispatchRegistry = DispatchRegistry (GhcMap.Map TypeRep Unknown)


-- | Empty registry.
empty :: DispatchRegistry
empty = DispatchRegistry GhcMap.empty


-- | Register a dispatcher closure for a request type.
--
-- Re-registering the same type overwrites the previous closure.
register ::
  forall request.
  (Integration request, Typeable request, Typeable (Response request)) =>
  (request -> Task IntegrationError (Response request)) ->
  DispatchRegistry ->
  DispatchRegistry
register closure (DispatchRegistry m) = do
  let key = typeRep (Proxy @request)
  let erased = Unknown.fromValue closure
  DispatchRegistry (GhcMap.insert key erased m)


-- | Look up a dispatcher closure for a request type.
--
-- Returns 'Nothing' when:
--
-- * The type was never registered.
-- * The stored closure's erased type does not match @request -> Task _ _@
--   (guards against 'unsafeCoerce' slippage).
lookup ::
  forall request.
  (Integration request, Typeable request, Typeable (Response request)) =>
  DispatchRegistry ->
  Maybe (request -> Task IntegrationError (Response request))
lookup (DispatchRegistry m) = do
  let key = typeRep (Proxy @request)
  case GhcMap.lookup key m of
    Nothing -> Nothing
    Just erased -> Unknown.toValue erased


-- | Count of registrations.
size :: DispatchRegistry -> Int
size (DispatchRegistry m) = GhcMap.size m
