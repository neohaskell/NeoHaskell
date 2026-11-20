-- | Exercise the compiler rejection for a payload absent from the event marker.
--
-- The standalone compiler check requires an EventVariantOf diagnostic.
module Service.Event.Compile.UnlistedPayload where

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


data Unlisted = Unlisted


unlistedEvent :: ProposalEvent
unlistedEvent = fromVariant Unlisted
