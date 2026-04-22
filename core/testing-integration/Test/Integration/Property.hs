-- | 'fakeProperty' — user-facing QuickCheck helper.
module Test.Integration.Property (fakeProperty) where

import Basics
import Service.Integration.Adapter (Integration (..))
import Task qualified
import Test.QuickCheck qualified as QuickCheck


-- | Run a QuickCheck property against 'runFake'. The invariant receives both
-- the generated request and the fake response.
fakeProperty ::
  forall request.
  ( Integration request,
    QuickCheck.Arbitrary request,
    Show request
  ) =>
  (request -> Response request -> QuickCheck.Property) ->
  QuickCheck.Property
fakeProperty check =
  QuickCheck.property
    ( \req ->
        QuickCheck.ioProperty
          ( Task.runOrPanic
              ( do
                  response <- runFake req
                  Task.yield (check req response)
              )
          )
    )
