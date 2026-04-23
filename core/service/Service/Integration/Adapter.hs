{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Integration.Adapter
  ( Integration (..),
    mkDispatcher,
  )
where

import Array qualified
import Basics
import Service.Integration.IntegrationError (IntegrationError)
import Service.Integration.Selection (Selection (..))
import Task (Task)
import Task qualified
import TypeName qualified


class Integration request where
  type Response request :: Type

  runReal :: request -> Task IntegrationError (Response request)

  runFake :: request -> Task IntegrationError (Response request)


-- | Build a pre-bound dispatcher closure for a request type based on the
-- active selection. Hybrid mode uses the integration's type name to decide.
--
-- @Real@   → 'runReal'
-- @Fake@   → 'runFake'
-- @Hybrid@ → 'runFake' when the type name is in the fake list, else 'runReal'
mkDispatcher ::
  forall request.
  (Integration request, TypeName.Inspectable request) =>
  Selection ->
  (request -> Task IntegrationError (Response request))
mkDispatcher selection = do
  let name = TypeName.reflect @request
  case selection of
    Real -> runReal
    Fake -> runFake
    Hybrid fakes ->
      if Array.contains name fakes
        then runFake
        else runReal
{-# INLINE mkDispatcher #-}
