-- | Fast immutable sets. A set is a collection of unique values.
module Set (
  -- * Sets
  Set,

  -- * Creation
  empty,
  wrap,
  singleton,
  fromLinkedList,
  fromArray,

  -- * Query
  isEmpty,
  size,
  contains,
  member,

  -- * Manipulate
  insert,
  remove,
  union,
  intersection,
  difference,

  -- * LinkedLists
  toLinkedList,

  -- * Arrays
  toArray,

  -- * Transform
  map,
  takeIf,
  dropIf,
  reduce,
  foldl,

  -- * Compatibility
  unwrap,
  fromLegacy,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Default (Default (..))
import Data.Set qualified
import LinkedList (LinkedList)
import Prelude qualified


-- | Representation of fast immutable sets. You can create sets of integers
-- (@Set Int@) or strings (@Set String@) or any other type of value you can
-- dream up. The values must support equality and ordering.
newtype Set a = Set (Data.Set.Set a)
  deriving (Prelude.Eq, Prelude.Show, Prelude.Ord, Generic)


instance (Prelude.Ord a) => Default (Set a) where
  def = Set.empty


-- | Helper function to unwrap a set
unwrap :: Set a -> Data.Set.Set a
unwrap (Set s) = s


-- | Return an empty set.
--
-- >>> empty :: Set Int
-- Set []
empty :: forall (value :: Type). (Prelude.Ord value) => Set value
empty = Set Data.Set.empty


-- | Determine if a set is empty.
--
-- >>> isEmpty empty
-- True
-- >>> isEmpty (singleton (0 :: Int))
-- False
isEmpty :: forall (value :: Type). Set value -> Bool
isEmpty = unwrap .> Data.Set.null


-- | Return the number of elements in a set.
--
-- >>> size (fromLinkedList [1,2,3] :: Set Int)
-- 3
-- >>> size empty
-- 0
size :: forall (value :: Type). Set value -> Int
size = unwrap .> Data.Set.size .> Prelude.fromIntegral


-- | Wraps an element into a set with a single element.
-- >>> wrap (0 :: Int)
-- Set [0]
wrap :: forall (value :: Type). (Prelude.Ord value) => value -> Set value
wrap = singleton


-- | Create a set with a single element.
-- >>> singleton (0 :: Int)
-- Set [0]
singleton :: forall (value :: Type). (Prelude.Ord value) => value -> Set value
singleton element = Set (Data.Set.singleton element)


-- | Create a set from a 'LinkedList'.
--
-- >>> fromLinkedList [1,2,3,2,1] :: Set Int
-- Set [1,2,3]
fromLinkedList :: forall (value :: Type). (Prelude.Ord value) => LinkedList value -> Set value
fromLinkedList = Data.Set.fromList .> Set


-- | Create a set from an 'Array'.
--
-- >>> fromArray ([1,2,3,2,1] :: Array Int) :: Set Int
-- Set [1,2,3]
fromArray :: forall (value :: Type). (Prelude.Ord value) => Array value -> Set value
fromArray array = do
  let linkedList = Array.toLinkedList array
  fromLinkedList linkedList


-- | Insert an element into a set.
--
-- >>> insert 4 (fromLinkedList [1,2,3] :: Set Int)
-- Set [1,2,3,4]
-- >>> insert 2 (fromLinkedList [1,2,3] :: Set Int)
-- Set [1,2,3]
insert :: forall (value :: Type). (Prelude.Ord value) => value -> Set value -> Set value
insert element set = do
  let dataSet = unwrap set
  Set (Data.Set.insert element dataSet)


-- | Remove an element from a set. If the element is not present, the set is unchanged.
--
-- >>> remove 2 (fromLinkedList [1,2,3] :: Set Int)
-- Set [1,3]
-- >>> remove 4 (fromLinkedList [1,2,3] :: Set Int)
-- Set [1,2,3]
remove :: forall (value :: Type). (Prelude.Ord value) => value -> Set value -> Set value
remove element set = do
  let dataSet = unwrap set
  Set (Data.Set.delete element dataSet)


-- | Check if a value is in the set.
--
-- >>> contains 2 (fromLinkedList [1,2,3] :: Set Int)
-- True
-- >>> contains 4 (fromLinkedList [1,2,3] :: Set Int)
-- False
contains :: forall (value :: Type). (Prelude.Ord value) => value -> Set value -> Bool
contains element set = do
  let dataSet = unwrap set
  Data.Set.member element dataSet


-- | Check if a value is a member of the set. Alias for 'contains'.
--
-- >>> member 2 (fromLinkedList [1,2,3] :: Set Int)
-- True
member :: forall (value :: Type). (Prelude.Ord value) => value -> Set value -> Bool
member = contains


-- | Combine two sets into a new set containing all elements from both.
--
-- >>> union (fromLinkedList [1,2] :: Set Int) (fromLinkedList [2,3] :: Set Int)
-- Set [1,2,3]
union :: forall (value :: Type). (Prelude.Ord value) => Set value -> Set value -> Set value
union other self = do
  let selfSet = unwrap self
  let otherSet = unwrap other
  Set (Data.Set.union selfSet otherSet)


-- | Get the intersection of two sets (elements present in both).
--
-- >>> intersection (fromLinkedList [1,2,3] :: Set Int) (fromLinkedList [2,3,4] :: Set Int)
-- Set [2,3]
intersection :: forall (value :: Type). (Prelude.Ord value) => Set value -> Set value -> Set value
intersection other self = do
  let selfSet = unwrap self
  let otherSet = unwrap other
  Set (Data.Set.intersection selfSet otherSet)


-- | Get the difference of two sets (elements in the first but not the second).
--
-- >>> difference (fromLinkedList [1,2,3] :: Set Int) (fromLinkedList [2,3,4] :: Set Int)
-- Set [1]
difference :: forall (value :: Type). (Prelude.Ord value) => Set value -> Set value -> Set value
difference other self = do
  let selfSet = unwrap self
  let otherSet = unwrap other
  Set (Data.Set.difference selfSet otherSet)


-- | Convert a set to a 'LinkedList'. The elements will be in ascending order.
--
-- >>> toLinkedList (fromLinkedList [3,1,2] :: Set Int)
-- [1,2,3]
toLinkedList :: forall (value :: Type). Set value -> LinkedList value
toLinkedList = unwrap .> Data.Set.toList


-- | Convert a set to an 'Array'. The elements will be in ascending order.
--
-- >>> toArray (fromLinkedList [3,1,2] :: Set Int)
-- Array [1,2,3]
toArray :: forall (value :: Type). Set value -> Array value
toArray set = do
  let linkedList = toLinkedList set
  Array.fromLinkedList linkedList


-- | Apply a function to every element in a set.
--
-- >>> map (\x -> x * 2) (fromLinkedList [1,2,3] :: Set Int)
-- Set [2,4,6]
map :: forall (a :: Type) (b :: Type). (Prelude.Ord b) => (a -> b) -> Set a -> Set b
map f set = do
  let dataSet = unwrap set
  Set (Data.Set.map f dataSet)


-- | Keep elements that pass the test.
--
-- >>> takeIf Basics.isEven (fromLinkedList [1,2,3,4,5,6] :: Set Int)
-- Set [2,4,6]
takeIf :: forall (value :: Type). (value -> Bool) -> Set value -> Set value
takeIf predicate set = do
  let dataSet = unwrap set
  Set (Data.Set.filter predicate dataSet)


-- | Drop elements that pass the test.
--
-- >>> dropIf Basics.isEven (fromLinkedList [1,2,3,4,5,6] :: Set Int)
-- Set [1,3,5]
dropIf :: forall (value :: Type). (value -> Bool) -> Set value -> Set value
dropIf predicate set = do
  let invertedPredicate = predicate .> not
  takeIf invertedPredicate set


-- | Reduce a set from the right.
--
-- >>> reduce (+) 0 (fromLinkedList [1,2,3] :: Set Int)
-- 6
reduce :: forall (a :: Type) (b :: Type). (a -> b -> b) -> b -> Set a -> b
reduce f initial set = do
  let dataSet = unwrap set
  Data.Set.foldr f initial dataSet


-- | Reduce a set from the left.
--
-- >>> foldl (+) 0 (fromLinkedList [1,2,3] :: Set Int)
-- 6
foldl :: forall (a :: Type) (b :: Type). (a -> b -> b) -> b -> Set a -> b
foldl f initial set = do
  let dataSet = unwrap set
  Data.Set.foldl' (Prelude.flip f) initial dataSet


-- | Convert a Haskell Set to a NeoHaskell Set.
-- Only use this for compatibility with legacy code.
-- >>> fromLegacy (Data.Set.fromList [1,2,3] :: Data.Set.Set Int)
-- Set [1,2,3]
fromLegacy :: forall (value :: Type). Data.Set.Set value -> Set value
fromLegacy = Set
