module Appendable (
  Appendable,
  Data.Semigroup.Semigroup (..),
  (++),
) where

import Data.Semigroup qualified


type Appendable = Data.Semigroup.Semigroup


infixr 5 ++


-- | Put two appendable things together. This includes arrays, lists, and text.
--
-- > "hello" ++ "world" == "helloworld"
-- > [1,1,2] ++ [3,5,8] == [1,1,2,3,5,8]
(++) :: (Appendable appendable) => appendable -> appendable -> appendable
(++) =
  (Data.Semigroup.<>)
