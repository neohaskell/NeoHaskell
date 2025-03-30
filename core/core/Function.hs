module Function (unchanged) where


-- | This function returns its input unchanged. It is what in other languages is called the identity function.
unchanged :: value -> value
unchanged value = value
{-# INLINE unchanged #-}
