module Unit (
  Unit,
  unit,
) where


-- | 'Unit' is a type that has a single value.
-- It is used to represent the return type of
-- functions that do not return anything.
-- In other languages it is usually called `void`.
type Unit = ()


unit :: Unit
unit = ()
