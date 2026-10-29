{-| Create protected values in command decisions and carry them through events and projections.
Use 'new' once per independent value and 'reference' only for application-owned administration queries.
-}
module Erasable (
  Erasable,
  new,
  map,
  reference,
) where

import Basics
import Decider (Decision (..))
import Decider qualified
import Erasable.Internal (ProtectedValue (..))
import Json qualified
import Schema qualified
import Text (Text)


newtype Erasable value = Erasable ProtectedValue


instance Show (Erasable value) where
  show _ = "Erasable <REDACTED>"


instance Json.ToJSON (Erasable value) where
  toJSON (Erasable protected) = Json.toJSON protected


instance Json.FromJSON (Erasable value) where
  parseJSON jsonValue = do
    protected <- Json.parseJSON jsonValue
    Json.yield (Erasable protected)


instance (Schema.ToSchema value) => Schema.ToSchema (Erasable value) where
  toSchema = Schema.SUnion [("Available", Schema.toSchema @value), ("Erased", Schema.SNull)]


new :: (Json.ToJSON value) => value -> Decision (Erasable value)
new value = do
  protected <- Decider.protectValue (Json.toJSON value)
  Return (Erasable protected)


map :: (Json.FromJSON input, Json.ToJSON output) => (input -> output) -> Erasable input -> Decision (Erasable output)
map transform (Erasable protected) = do
  transformed <- Decider.transformValue transform protected
  Return (Erasable transformed)


reference :: Erasable value -> Text
reference (Erasable protected) = protected.reference
