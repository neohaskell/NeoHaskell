-- | DEPRECATED: Use Decider module instead.
--
-- This module is kept for backward compatibility during the transition.
module Decision (
  Decision,
  generateUuid,
  acceptAny,
  acceptNew,
  acceptExisting,
  acceptAfter,
  reject,
) where

import Decider (
  Decision,
  acceptAfter,
  acceptAny,
  acceptExisting,
  acceptNew,
  generateUuid,
  reject,
 )
