{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.Definition.TypeLevel (
  -- * Type-level list operations
  Member,
  Union,
  Difference,
) where

import Basics

-- | Type family to check if a symbol is a member of a type-level list
-- Returns 'True if the element is in the list, 'False otherwise
type family Member (element :: Symbol) (list :: [Symbol]) :: Bool where
  Member element '[] = 'False
  Member element (element ': rest) = 'True
  Member element (other ': rest) = Member element rest

-- | Type family to compute the union of two type-level lists
-- Eliminates duplicates from the result
type family Union (list1 :: [Symbol]) (list2 :: [Symbol]) :: [Symbol] where
  Union '[] list2 = list2
  Union (head ': tail) list2 =
    If (Member head list2)
      (Union tail list2)
      (head ': Union tail list2)

-- | Helper type family for conditional type-level computation
type family If (condition :: Bool) (ifTrue :: value) (ifFalse :: value) :: value where
  If 'True ifTrue ifFalse = ifTrue
  If 'False ifTrue ifFalse = ifFalse

-- | Type family to compute the difference of two type-level lists
-- Returns elements present in the first list but absent from the second
type family Difference (list1 :: [Symbol]) (list2 :: [Symbol]) :: [Symbol] where
  Difference '[] list2 = '[]
  Difference (head ': tail) list2 =
    If (Member head list2)
      (Difference tail list2)
      (head ': Difference tail list2)