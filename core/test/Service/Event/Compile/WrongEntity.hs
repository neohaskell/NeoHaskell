-- | Exercise the compiler rejection for a payload owned by another entity.
--
-- The standalone compiler check requires the marker's ownership diagnostic.
module Service.Event.Compile.WrongEntity where

import Core


data Proposal = Proposal


data Other = Other


data ForeignPayload = ForeignPayload


type instance EntityOf ForeignPayload = Other


deriveEntityEvents ''Proposal [''ForeignPayload]
