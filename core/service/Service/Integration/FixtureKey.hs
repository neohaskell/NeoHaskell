-- | Newtype wrapper around the canonical request hash.
--
-- Prevents accidental misuse: a raw 'Text' cannot stand in for a fixture key
-- at the API boundary.
module Service.Integration.FixtureKey
  ( FixtureKey (..),
    fromRequest,
    fromHash,
    toText,
  )
where

import Basics
import Data.Aeson qualified as Aeson
import Result (Result (..))
import Service.Integration.Canonical qualified as Canonical
import Service.Integration.IntegrationError (IntegrationError)
import Text (Text)


newtype FixtureKey = FixtureKey Text
  deriving (Eq, Ord, Show, Generic)


-- | Derive a 'FixtureKey' from an 'Aeson.ToJSON' request via canonical-JSON
-- SHA-256 hashing. Propagates 'IntegrationError.ValidationFailure' when the
-- request contains 'NaN' or 'Infinity'.
fromRequest ::
  forall request.
  (Aeson.ToJSON request) =>
  request ->
  Result IntegrationError FixtureKey
fromRequest request =
  case Canonical.hash request of
    Err err -> Err err
    Ok hashText -> Ok (FixtureKey hashText)


-- | Build a 'FixtureKey' from an already-computed hash string. Internal use
-- only; callers are responsible for ensuring validity.
fromHash :: Text -> FixtureKey
fromHash = FixtureKey


toText :: FixtureKey -> Text
toText (FixtureKey hex) = hex
