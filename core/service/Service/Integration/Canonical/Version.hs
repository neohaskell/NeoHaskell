-- | Versioning constant for the first-party RFC 8785 canonical-JSON encoder.
--
-- A future bug-fix release may bump this so fixture hashes can be invalidated
-- explicitly per ADR-0055 §12.
module Service.Integration.Canonical.Version (version) where

import Basics


version :: Int
version = 1
