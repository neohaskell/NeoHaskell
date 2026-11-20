-- | Compile a consumer that lists its event payload in the entity event marker.
--
-- The standalone compiler check uses this module as the accepted control.
module Service.Event.Compile.ValidPayload where

import Core
import Service.Entity.Core qualified as Entity
import Uuid qualified


data Proposal = Proposal


instance Entity Proposal where
  initialStateImpl = Proposal
  updateImpl _ proposal = proposal


data ProposalCreated = ProposalCreated
  deriving (Show)


deriveEvent ''ProposalCreated


type instance EntityOf ProposalCreated = Proposal


instance Entity.Event ProposalCreated where
  getEventEntityIdImpl _ = Uuid.nil


deriveEntityEvents ''Proposal [''ProposalCreated]


createdEvent :: ProposalEvent
createdEvent = fromVariant ProposalCreated
