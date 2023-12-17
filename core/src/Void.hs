module Void (
  Void,
  void,
) where


-- | 'Void' is a type that has a single value.
-- It is used to represent the return type of
-- functions that do not return anything.
-- It has the exact meaning of the `void` type
-- in other languages.
type Void = ()


void :: Void
void = ()