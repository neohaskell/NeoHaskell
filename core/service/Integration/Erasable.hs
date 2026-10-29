{-# LANGUAGE UndecidableInstances #-}

{-| Erase one protected reference through the built-in outbound action flow.
Use 'Erase' from an application event handler and record the result command in the application's own entity.
-}
module Integration.Erasable (
  Erase (..),
  ErasureTarget (..),
  ErasureResult (..),
) where

import Basics
import GHC.TypeLits qualified as GHC
import Integration qualified
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.EventStore.Core (ErasureResult (..), ErasureTarget (..))
import Task qualified


data Erase command = Erase
  { target :: ErasureTarget,
    onResult :: ErasureResult -> command,
    onFailure :: Integration.IntegrationError -> command
  }


instance
  (Json.ToJSON command, GHC.KnownSymbol (NameOf command)) =>
  Integration.ToAction (Erase command)
  where
  toAction config = Integration.action \context -> do
    case context.eraseErasableKey of
      Nothing -> Integration.emitCommand (config.onFailure (Integration.UnexpectedError "Erasable key store unavailable"))
      Just erase -> do
        outcome <- erase config.target |> Task.asResult
        case outcome of
          Ok result -> Integration.emitCommand (config.onResult result)
          Err error -> Integration.emitCommand (config.onFailure error)
