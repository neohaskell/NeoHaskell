module Decision (
  Decision,
  generateUuid,
  acceptAny,
  acceptNew,
  acceptExisting,
  acceptAfter,
  reject,
) where

import Array (Array)
import Service.Command.Core (Decision (..))
import Service.Event (InsertionType (..), StreamPosition)
import Text (Text)
import Uuid (Uuid)


generateUuid :: Decision Uuid
generateUuid = GenUuid


acceptAny :: (Array a) -> Decision a
acceptAny events =
  Accept AnyStreamState events


acceptNew :: (Array a) -> Decision a
acceptNew events =
  Accept StreamCreation events


acceptExisting :: (Array a) -> Decision a
acceptExisting events =
  Accept ExistingStream events


acceptAfter :: StreamPosition -> (Array a) -> Decision a
acceptAfter pos events =
  Accept (InsertAfter pos) events


reject :: Text -> Decision a
reject reason = Reject reason