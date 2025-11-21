module Decision (
  Decision (..),
  DecisionContext (..),
  generateUuid,
  accept,
  reject,
  runDecision,
) where

import Applicable
import Array (Array)
import Basics
import Control.Monad qualified as Monad
import Mappable
import Service.Command.Core (CommandResult (..))
import Service.Event (InsertionType)
import Task (Task)
import Task qualified
import Text (Text)
import Thenable
import Uuid (Uuid)


data Decision a where
  Return :: a -> Decision a
  Bind :: Decision a -> (a -> Decision b) -> Decision b
  GenUuid :: Decision Uuid
  Accept :: InsertionType -> Array a -> Decision a
  Reject :: Text -> Decision a


instance Functor Decision where
  fmap f m = m |> Thenable.andThen (\r -> f r |> Return)


instance Applicative Decision where
  pure = Return
  (<*>) = Monad.ap


instance Monad Decision where
  m >>= f = Bind m f


-- Smart constructors
generateUuid :: Decision Uuid
generateUuid = GenUuid


accept :: InsertionType -> (Array a) -> Decision a
accept stream events =
  Accept stream events


reject :: Text -> Decision a
reject reason = Reject reason


data DecisionContext = DecisionContext
  { genUuid :: Task Text Uuid
  }


runDecision :: (HasCallStack) => DecisionContext -> Decision a -> Task Text (CommandResult a)
runDecision ctx = go
 where
  go :: forall a. (HasCallStack) => Decision a -> Task Text (CommandResult a)
  go (Return _) = Task.throw "Decision didn't terminate with accept/reject"
  go (Bind m f) = case m of
    GenUuid -> do
      uuid <- ctx.genUuid
      f uuid |> go
    Accept _ _ -> Task.throw "Accept must be the last statement"
    Reject _ -> Task.throw "Reject must be the last statement"
    Return a -> go (f a)
    Bind m' f' -> go (Bind m' (\x -> Bind (f' x) f))
  go GenUuid = Task.throw "Unbound GenUuid"
  go (Accept s events) = AcceptCommand s events |> Task.yield
  go (Reject reason) = RejectCommand reason |> Task.yield