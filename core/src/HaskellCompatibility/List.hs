module HaskellCompatibility.List (
  HaskellList,
) where


-- | A type alias for the Haskell list type.
-- It is exposed like this to ensure that it's
-- usage is explicit, and we make sure that
-- it is only used for compatibility with
-- Haskell.
type HaskellList item = [item]