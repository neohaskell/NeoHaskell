module LazySequence (LazySequence) where


-- | A `LazySequence` represents a series of elements that are computed only when they are needed, rather than all at once. This concept is akin to 'lazy evaluation' in the functional programming realm.
-- | In TypeScript, this might be similar to using generators to create sequences whose computations are deferred until the elements are actually iterated over.
-- |
-- | Here's how you might initialize and use a `LazySequence`:
-- |
-- | >>> let seq = [1..]
-- | >>> take 5 seq
-- | [1, 2, 3, 4, 5]
type LazySequence value = [value]
