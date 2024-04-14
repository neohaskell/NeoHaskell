module LazySequence (
  LazySequence (..),
) where


-- | A `LazySequence` represents a series of elements that are computed only when they are needed.
-- |
-- | Here's how you might initialize and use a `LazySequence`:
-- |
-- | >>> let seq = [1..]
-- | >>> take 5 seq
-- | [1, 2, 3, 4, 5]
newtype LazySequence value = INTERNAL_CORE_LAZY_SEQUENCE_CONSTRUCTOR [value]
