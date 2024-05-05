-- | Fast immutable arrays. The elements in an array must have the same type.
module Array (
  -- * Arrays
  Array,

  -- * Creation
  empty,
  initialize,
  repeat,
  fromList,

  -- * Query
  isEmpty,
  length,
  get,

  -- * Manipulate
  set,
  push,
  append,
  slice,

  -- * LinkedLists
  toList,
  toIndexedLinkedList,

  -- * Transform
  map,
  indexedMap,
  foldr,
  foldl,
  filter,
) where

import Basics (
  Bool,
  Int,
  clamp,
  (&&),
  (+),
  (-),
  (<),
  (<=),
  (<|),
  (>>),
 )
import Data.Foldable qualified
import Data.Vector ((!?), (++), (//))
import Data.Vector qualified
import Int
import LinkedList (LinkedList)
import LinkedList qualified
import Maybe (Maybe (..))
import Pipe
import Tuple qualified
import Prelude (otherwise)
import Prelude qualified


-- | Representation of fast immutable arrays. You can create arrays of integers
-- (@Array Int@) or strings (@Array String@) or any other type of value you can
-- dream up.
newtype Array a = Array (Data.Vector.Vector a)
  deriving (Prelude.Eq, Prelude.Show)


-- | Helper function to unwrap an array
unwrap :: Array a -> Data.Vector.Vector a
unwrap (Array v) = v


-- | Return an empty array.
--
-- > length empty == 0
empty :: Array a
empty =
  Array Data.Vector.empty


-- | Determine if an array is empty.
--
-- > isEmpty empty == True
isEmpty :: Array a -> Bool
isEmpty = unwrap .> Data.Vector.null


-- | Return the length of an array.
--
-- > length (fromList [1,2,3]) == 3
length :: Array a -> Int
length =
  unwrap
    .> Data.Vector.length
    .> Prelude.fromIntegral


-- | Initialize an array. @initialize n f@ creates an array of length @n@ with
-- the element at index @i@ initialized to the result of @(f i)@.
--
-- > initialize 4 identity    == fromList [0,1,2,3]
-- > initialize 4 (\n -> n*n) == fromList [0,1,4,9]
-- > initialize 4 (always 0)  == fromList [0,0,0,0]
initialize :: Int -> (Int -> a) -> Array a
initialize n f =
  Array
    <| Data.Vector.generate
      (Prelude.fromIntegral n)
      (Prelude.fromIntegral .> f)


-- | Creates an array with a given length, filled with a default element.
--
-- > repeat 5 0     == fromList [0,0,0,0,0]
-- > repeat 3 "cat" == fromList ["cat","cat","cat"]
--
-- Notice that @repeat 3 x@ is the same as @initialize 3 (always x)@.
repeat :: Int -> a -> Array a
repeat n e =
  Array
    <| Data.Vector.replicate (Prelude.fromIntegral n) e


-- | Create an array from a 'LinkedList'.
fromList :: LinkedList a -> Array a
fromList =
  Data.Vector.fromList .> Array


-- | Return @Just@ the element at the index or @Nothing@ if the index is out of range.
--
-- > get  0   (fromList [0,1,2]) == Just 0
-- > get  2   (fromList [0,1,2]) == Just 2
-- > get  5   (fromList [0,1,2]) == Nothing
-- > get (-1) (fromList [0,1,2]) == Nothing
get :: Int -> Array a -> Maybe a
get i array =
  unwrap array !? Prelude.fromIntegral i


-- | Set the element at a particular index. Returns an updated array.
--
-- If the index is out of range, the array is unaltered.
--
-- > set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
set :: Int -> a -> Array a -> Array a
set i value array = Array result
 where
  len = length array
  vector = unwrap array
  result
    | 0 <= i && i < len = vector // [(Prelude.fromIntegral i, value)]
    | otherwise = vector


-- | Push an element onto the end of an array.
--
-- > push 3 (fromList [1,2]) == fromList [1,2,3]
push :: a -> Array a -> Array a
push a (Array vector) =
  Array (Data.Vector.snoc vector a)


-- | Create a list of elements from an array.
--
-- > toList (fromList [3,5,8]) == [3,5,8]
toList :: Array a -> LinkedList a
toList = unwrap .> Data.Vector.toList


-- | Create an indexed list from an array. Each element of the array will be
-- paired with its index.
--
-- > toIndexedLinkedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
toIndexedLinkedList :: Array a -> LinkedList (Int, a)
toIndexedLinkedList =
  unwrap
    .> Data.Vector.indexed
    .> Data.Vector.toList
    .> LinkedList.map (Tuple.mapFirst Prelude.fromIntegral)


-- | Reduce an array from the right. Read @foldr@ as fold from the right.
--
-- > foldr (+) 0 (repeat 3 5) == 15
foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f value array = Prelude.foldr f value (unwrap array)


-- | Reduce an array from the left. Read @foldl@ as fold from the left.
--
-- > foldl (:) [] (fromList [1,2,3]) == [3,2,1]
foldl :: (a -> b -> b) -> b -> Array a -> b
foldl f value array =
  Data.Foldable.foldl' (\a b -> f b a) value (unwrap array)


-- | Keep elements that pass the test.
--
-- > filter isEven (fromList [1,2,3,4,5,6]) == (fromList [2,4,6])
filter :: (a -> Bool) -> Array a -> Array a
filter f (Array vector) =
  Array (Data.Vector.filter f vector)


-- | Apply a function on every element in an array.
--
-- > map sqrt (fromList [1,4,9]) == fromList [1,2,3]
map :: (a -> b) -> Array a -> Array b
map f (Array vector) =
  Array (Data.Vector.map f vector)


-- | Apply a function on every element with its index as first argument.
--
-- > indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
indexedMap :: (Int -> a -> b) -> Array a -> Array b
indexedMap f (Array vector) =
  Array (Data.Vector.imap (Prelude.fromIntegral .> f) vector)


-- | Append two arrays to a new one.
--
-- > append (repeat 2 42) (repeat 3 81) == fromList [42,42,81,81,81]
append :: Array a -> Array a -> Array a
append (Array first) (Array second) =
  Array (first ++ second)


-- | Get a sub-section of an array: @(slice start end array)@. The @start@ is a
-- zero-based index where we will start our slice. The @end@ is a zero-based index
-- that indicates the end of the slice. The slice extracts up to but not including
-- @end@.
--
-- > slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
-- > slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]
--
-- Both the @start@ and @end@ indexes can be negative, indicating an offset from
-- the end of the array.
--
-- > slice  1    (-1) (fromList [0,1,2,3,4]) == fromList [1,2,3]
-- > slice  (-2) 5    (fromList [0,1,2,3,4]) == fromList [3,4]
--
-- This makes it pretty easy to @pop@ the last element off of an array:
-- @slice 0 -1 array@
slice :: Int -> Int -> Array a -> Array a
slice from to (Array vector)
  | sliceLen <= 0 = empty
  | otherwise = Array <| Data.Vector.slice from' sliceLen vector
 where
  len = Data.Vector.length vector
  handleNegative value
    | value < 0 = len + value
    | otherwise = value
  normalize =
    Prelude.fromIntegral
      .> handleNegative
      .> clamp 0 len
  from' = normalize from
  to' = normalize to
  sliceLen = to' - from'