module Service.Entity.TH.NoShowFixture where

import Core
import Json qualified
import Language.Haskell.TH.Syntax qualified as TH
import Uuid qualified

-- An entity field can be serializable without being printable.
data QuietValue = QuietValue {amount :: Int}
  deriving (Generic)

instance Json.FromJSON QuietValue
instance Json.ToJSON QuietValue

data QuietEvent = QuietChanged {amount :: Int}

deriveEvent ''QuietEvent

data QuietEntity = QuietEntity {secret :: QuietValue}

initialState :: QuietEntity
initialState = QuietEntity {secret = QuietValue {amount = 0}}

update :: QuietEvent -> QuietEntity -> QuietEntity
update change _ = QuietEntity {secret = QuietValue {amount = change.amount}}

getEventEntityId :: QuietEvent -> Uuid
getEventEntityId _ = Uuid.nil

deriveEntity ''QuietEntity ''QuietEvent

$(do
    instances <- TH.reifyInstances ''Show [TH.ConT ''QuietEntity]
    case instances of
      [] -> [d| hasEntityShow :: Bool; hasEntityShow = False |]
      _ : _ -> [d| hasEntityShow :: Bool; hasEntityShow = True |])
