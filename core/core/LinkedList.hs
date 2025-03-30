-- |  You can create a @LinkedList@ in Elm with the @[1,2,3]@ syntax, so lists are used all over the place. This module has a bunch of functions to help you work with them!
module LinkedList (
  LinkedList,

  -- * Create
  singleton,
  repeat,
  range,

  -- * Transform
  map,
  indexedMap,
  foldl,
  foldr,
  filter,
  filterMap,

  -- * Utilities
  length,
  reverse,
  member,
  all,
  any,
  maximum,
  minimum,
  sum,
  product,

  -- * Combine
  append,
  concat,
  concatMap,
  intersperse,
  map2,
  map3,
  map4,
  map5,

  -- * Sort
  sort,
  sortBy,
  sortWith,

  -- * Deconstruct
  isEmpty,
  head,
  tail,
  take,
  drop,
  partition,
  unzip,
  get,
) where

import Basics
import Data.Foldable qualified
import Data.List qualified
import Data.Maybe qualified
import Mappable qualified
import Maybe (Maybe (..))
import Thenable qualified
import Prelude qualified


-- | In Haskell a list type is defined using square brackets. This alias allows
-- us to alternatively write the type like we would in Elm.
type LinkedList a = [a]


-- CREATE

-- | Create a list with only one element:
--
-- > singleton 1234 == [1234]
-- > singleton "hi" == ["hi"]
singleton :: a -> LinkedList a
singleton value = [value]


-- | Create a list with *n* copies of a value:
--
-- > repeat 3 (0,0) == [(0,0),(0,0),(0,0)]
repeat :: Int -> a -> LinkedList a
repeat =
  Prelude.fromIntegral .> Data.List.replicate


-- | Create a list of numbers, every element increasing by one.
--
-- You give the lowest and highest number that should be in the list.
--
-- > range 3 6 == [3, 4, 5, 6]
-- > range 3 3 == [3]
-- > range 6 3 == []
range :: Int -> Int -> LinkedList Int
range lo hi =
  [lo .. hi]


-- TRANSFORM

-- | Apply a function to every element of a list.
--
-- > map sqrt [1,4,9] == [1,2,3]
-- > map not [True,False,True] == [False,True,False]
--
-- So @map func [ a, b, c ]@ is the same as @[ func a, func b, func c ]@
map :: (a -> b) -> LinkedList a -> LinkedList b
map =
  Mappable.map


-- | Same as @map@ but the function is also applied to the index of each element
-- (starting at zero).
--
-- > indexedMap Tuple.pair ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
indexedMap :: (Int -> a -> b) -> LinkedList a -> LinkedList b
indexedMap f xs =
  map2 f [0 .. (length xs - 1)] xs


-- | Reduce a list from the left.
--
-- > foldl (+)  0  [1,2,3] == 6
-- > foldl (::) [] [1,2,3] == [3,2,1]
--
-- So @foldl step state [1,2,3]@ is like saying:
--
-- > state
-- >   |> step 1
-- >   |> step 2
-- >   |> step 3
--
-- Note: This function is implemented using fold' to eagerly evaluate the
-- accumulator, preventing space leaks.
foldl :: (a -> b -> b) -> b -> LinkedList a -> b
foldl func =
  Data.List.foldl' (\a b -> func b a)


-- | Reduce a list from the right.
--
-- > foldr (+)  0  [1,2,3] == 6
-- > foldr (::) [] [1,2,3] == [1,2,3]
--
-- So @foldr step state [1,2,3]@ is like saying:
--
-- > state
-- >   |> step 3
-- >   |> step 2
-- >   |> step 1
foldr :: (a -> b -> b) -> b -> LinkedList a -> b
foldr =
  Data.List.foldr


-- | Keep elements that satisfy the test.
--
-- > filter isEven [1,2,3,4,5,6] == [2,4,6]
filter :: (a -> Bool) -> LinkedList a -> LinkedList a
filter =
  Data.List.filter


-- | Filter out certain values. For example, maybe you have a bunch of strings
-- from an untrusted source and you want to turn them into numbers:
--
-- > numbers : LinkedList Int
-- > numbers =
-- >   filterMap String.toInt ["3", "hi", "12", "4th", "May"]
-- > -- numbers == [3, 12]
filterMap :: (a -> Maybe b) -> LinkedList a -> LinkedList b
filterMap =
  Data.Maybe.mapMaybe


-- UTILITIES

-- | Determine the length of a list.
--
-- > length [1,2,3] == 3
length :: LinkedList a -> Int
length =
  Data.List.length .> Prelude.fromIntegral


-- | Reverse a list.
-- > reverse [1,2,3,4] == [4,3,2,1]
reverse :: LinkedList a -> LinkedList a
reverse =
  Data.List.reverse


-- | Figure out whether a list contains a value.
--
-- > member 9 [1,2,3,4] == False
-- > member 4 [1,2,3,4] == True
member :: (Prelude.Eq a) => a -> LinkedList a -> Bool
member =
  Data.List.elem


-- | Determine if all elements satisfy some test.
--
-- > all isEven [2,4] == True
-- > all isEven [2,3] == False
-- > all isEven [] == True
all :: (a -> Bool) -> LinkedList a -> Bool
all =
  Data.List.all


-- | Determine if any elements satisfy some test.
--
-- > any isEven [2,3] == True
-- > any isEven [1,3] == False
-- > any isEven [] == False
any :: (a -> Bool) -> LinkedList a -> Bool
any =
  Data.List.any


-- | Find the maximum element in a non-empty list.
--
-- > maximum [1,4,2] == Just 4
-- > maximum []      == Nothing
maximum :: (Ord a) => LinkedList a -> Maybe a
maximum list =
  case list of
    [] ->
      Nothing
    _ ->
      Just (Data.List.maximum list)


-- | Find the minimum element in a non-empty list.
--
-- > minimum [3,2,1] == Just 1
-- > minimum []      == Nothing
minimum :: (Ord a) => LinkedList a -> Maybe a
minimum list =
  case list of
    [] ->
      Nothing
    _ ->
      Just (Data.List.minimum list)


-- | Get the sum of the list elements.
--
-- > sum [1,2,3,4] == 10
sum :: (Num a) => LinkedList a -> a
sum =
  Data.Foldable.sum


-- | Get the product of the list elements.
--
-- > product [1,2,3,4] == 24
product :: (Num a) => LinkedList a -> a
product =
  Data.Foldable.product


-- COMBINE

-- | Put two lists together.
--
-- > append [1,1,2] [3,5,8] == [1,1,2,3,5,8]
-- > append ['a','b'] ['c'] == ['a','b','c']
--
-- You can also use the @(++)@ operator to append lists.
append :: LinkedList a -> LinkedList a -> LinkedList a
append =
  Prelude.mappend


-- | Concatenate a bunch of lists into a single list:
--
-- > concat [[1,2],[3],[4,5]] == [1,2,3,4,5]
concat :: LinkedList (LinkedList a) -> LinkedList a
concat =
  Prelude.mconcat


-- | Map a given function onto a list and flatten the resulting lists.
--
-- > concatMap f xs == concat (map f xs)
concatMap :: (a -> LinkedList b) -> LinkedList a -> LinkedList b
concatMap =
  Thenable.andThen


-- | Places the given value between all members of the given list.
--
-- > intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]
intersperse :: a -> LinkedList a -> LinkedList a
intersperse =
  Data.List.intersperse


-- | Combine two lists, combining them with the given function.
-- If one list is longer, the extra elements are dropped.
--
-- > totals : LinkedList Int -> LinkedList Int -> LinkedList Int
-- > totals xs ys =
-- >   LinkedList.map2 (+) xs ys
-- >
-- > -- totals [1,2,3] [4,5,6] == [5,7,9]
-- >
-- > pairs : LinkedList a -> LinkedList b -> LinkedList (a,b)
-- > pairs xs ys =
-- >   LinkedList.map2 Tuple.pair xs ys
-- >
-- > -- pairs ["alice","bob","chuck"] [2,5,7,8]
-- > --   == [("alice",2),("bob",5),("chuck",7)]
--
-- __Note:__ This behaves differently than 'NriPrelude.map2', which produces
-- all combinations of elements from both lists.
map2 :: (a -> b -> result) -> LinkedList a -> LinkedList b -> LinkedList result
map2 =
  Data.List.zipWith


-- | __Note:__ This behaves differently than 'NriPrelude.map3', which produces
-- all combinations of elements from all lists.
map3 :: (a -> b -> c -> result) -> LinkedList a -> LinkedList b -> LinkedList c -> LinkedList result
map3 =
  Data.List.zipWith3


-- | __Note:__ This behaves differently than 'NriPrelude.map4', which produces
-- all combinations of elements from all lists.
map4 ::
  (a -> b -> c -> d -> result) -> LinkedList a -> LinkedList b -> LinkedList c -> LinkedList d -> LinkedList result
map4 =
  Data.List.zipWith4


-- | __Note:__ This behaves differently than 'NriPrelude.map5', which produces
-- all combinations of elements from all lists.
map5 ::
  (a -> b -> c -> d -> e -> result) ->
  LinkedList a ->
  LinkedList b ->
  LinkedList c ->
  LinkedList d ->
  LinkedList e ->
  LinkedList result
map5 =
  Data.List.zipWith5


-- SORT

-- | Sort values from lowest to highest
--
-- > sort [3,1,5] == [1,3,5]
sort :: (Ord a) => LinkedList a -> LinkedList a
sort =
  Data.List.sort


-- | Sort values by a derived property.
--
-- > alice = { name="Alice", height=1.62 }
-- > bob   = { name="Bob"  , height=1.85 }
-- > chuck = { name="Chuck", height=1.76 }
-- >
-- > sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
-- > sortBy .height [chuck,alice,bob] == [alice,chuck,bob]
-- >
-- > sortBy String.length ["mouse","cat"] == ["cat","mouse"]
sortBy :: (Ord b) => (a -> b) -> LinkedList a -> LinkedList a
sortBy =
  Data.List.sortOn


-- | Sort values with a custom comparison function.
--
-- > sortWith flippedComparison [1,2,3,4,5] == [5,4,3,2,1]
-- > flippedComparison a b =
-- >     case compare a b of
-- >       LT -> GT
-- >       EQ -> EQ
-- >       GT -> LT
--
-- This is also the most general sort function, allowing you
-- to define any other: @sort == sortWith compare@
sortWith :: (a -> a -> Ordering) -> LinkedList a -> LinkedList a
sortWith =
  Data.List.sortBy


-- DECONSTRUCT

-- | Determine if a list is empty.
--
-- > isEmpty [] == True
--
-- __Note:__ It is usually preferable to use a @case@ to test this so you do not
-- forget to handle the @(x :: xs)@ case as well!
isEmpty :: LinkedList a -> Bool
isEmpty =
  Data.List.null


-- | Extract the first element of a list.
--
-- > head [1,2,3] == Just 1
-- > head [] == Nothing
--
-- __Note:__ It is usually preferable to use a @case@ to deconstruct a @LinkedList@
-- because it gives you @(x :: xs)@ and you can work with both subparts.
head :: LinkedList a -> Maybe a
head xs =
  case xs of
    x : _ ->
      Just x
    _ ->
      Nothing


-- | Extract the rest of the list.
--
-- > tail [1,2,3] == Just [2,3]
-- > tail [] == Nothing
--
-- __Note:__ It is usually preferable to use a @case@ to deconstruct a @LinkedList@
-- because it gives you @(x :: xs)@ and you can work with both subparts.
tail :: LinkedList a -> Maybe (LinkedList a)
tail list =
  case list of
    _ : xs ->
      Just xs
    _ ->
      Nothing


-- | Take the first *n* members of a list.
--
-- > take 2 [1,2,3,4] == [1,2]
take :: Int -> LinkedList a -> LinkedList a
take =
  Prelude.fromIntegral .> Data.List.take


-- | Drop the first *n* members of a list.
--
-- > drop 2 [1,2,3,4] == [3,4]
drop :: Int -> LinkedList a -> LinkedList a
drop =
  Prelude.fromIntegral .> Data.List.drop


-- | Partition a list based on some test. The first list contains all values
-- that satisfy the test, and the second list contains all the value that do not.
--
-- > partition (\x -> x < 3) [0,1,2,3,4,5] == ([0,1,2], [3,4,5])
-- > partition isEven        [0,1,2,3,4,5] == ([0,2,4], [1,3,5])
partition :: (a -> Bool) -> LinkedList a -> (LinkedList a, LinkedList a)
partition =
  Data.List.partition


-- | Decompose a list of tuples into a tuple of lists.
--
-- > unzip [(0, True), (17, False), (1337, True)] == ([0,17,1337], [True,False,True])
unzip :: LinkedList (a, b) -> (LinkedList a, LinkedList b)
unzip =
  Data.List.unzip


-- FIXME: Use safe functions for these
get :: Int -> LinkedList a -> Maybe a
get index list =
  case drop index list of
    x : _ ->
      Just x
    _ ->
      Nothing
