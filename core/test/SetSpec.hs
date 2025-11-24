module SetSpec where

import Array qualified
import Basics
import Core
import Data.Set qualified
import Set (Set)
import Set qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Set creation" do
    it "creates an empty set" \_ -> do
      let emptySet = Set.empty :: Set Int
      Set.isEmpty emptySet |> shouldBe True
      Set.size emptySet |> shouldBe 0

    it "wraps a single element" \_ -> do
      let singleSet = Set.wrap (5 :: Int)
      Set.isEmpty singleSet |> shouldBe False
      Set.size singleSet |> shouldBe 1
      Set.contains 5 singleSet |> shouldBe True

    it "creates a singleton set" \_ -> do
      let singleSet = Set.singleton (42 :: Int)
      Set.size singleSet |> shouldBe 1
      Set.contains 42 singleSet |> shouldBe True

    it "creates a set from a LinkedList" \_ -> do
      let linkedList = [1, 2, 3, 2, 1] :: LinkedList Int
      let setFromList = Set.fromLinkedList linkedList
      Set.size setFromList |> shouldBe 3
      Set.contains 1 setFromList |> shouldBe True
      Set.contains 2 setFromList |> shouldBe True
      Set.contains 3 setFromList |> shouldBe True

    it "creates a set from a LinkedList removing duplicates" \_ -> do
      let linkedList = [5, 5, 5, 5] :: LinkedList Int
      let setFromList = Set.fromLinkedList linkedList
      Set.size setFromList |> shouldBe 1
      Set.contains 5 setFromList |> shouldBe True

    it "creates a set from an Array" \_ -> do
      let array = Array.fromLinkedList [1, 2, 3, 2, 1] :: Array Int
      let setFromArray = Set.fromArray array
      Set.size setFromArray |> shouldBe 3
      Set.contains 1 setFromArray |> shouldBe True
      Set.contains 2 setFromArray |> shouldBe True
      Set.contains 3 setFromArray |> shouldBe True

    it "creates an empty set from an empty LinkedList" \_ -> do
      let emptyList = [] :: LinkedList Int
      let emptySet = Set.fromLinkedList emptyList
      Set.isEmpty emptySet |> shouldBe True

  describe "Set query operations" do
    it "checks if set is empty" \_ -> do
      let emptySet = Set.empty :: Set Int
      let nonEmptySet = Set.singleton (1 :: Int)
      Set.isEmpty emptySet |> shouldBe True
      Set.isEmpty nonEmptySet |> shouldBe False

    it "returns the correct size" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3] :: Set Int
      let set2 = Set.fromLinkedList [1, 2, 3, 4, 5] :: Set Int
      Set.size set1 |> shouldBe 3
      Set.size set2 |> shouldBe 5

    it "checks if element is contained (contains)" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3, 4, 5] :: Set Int
      Set.contains 3 testSet |> shouldBe True
      Set.contains 6 testSet |> shouldBe False
      Set.contains 1 testSet |> shouldBe True
      Set.contains 5 testSet |> shouldBe True

    it "checks if element is a member (member)" \_ -> do
      let testSet = Set.fromLinkedList [10, 20, 30] :: Set Int
      Set.member 20 testSet |> shouldBe True
      Set.member 40 testSet |> shouldBe False

    it "contains and member are equivalent" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      Set.contains 2 testSet |> shouldBe (Set.member 2 testSet)
      Set.contains 5 testSet |> shouldBe (Set.member 5 testSet)

  describe "Set manipulation operations" do
    it "inserts an element into a set" \_ -> do
      let initialSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let updatedSet = Set.insert 4 initialSet
      Set.size updatedSet |> shouldBe 4
      Set.contains 4 updatedSet |> shouldBe True

    it "inserting an existing element doesn't change size" \_ -> do
      let initialSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let updatedSet = Set.insert 2 initialSet
      Set.size updatedSet |> shouldBe 3
      Set.contains 2 updatedSet |> shouldBe True

    it "inserts element into empty set" \_ -> do
      let emptySet = Set.empty :: Set Int
      let updatedSet = Set.insert 10 emptySet
      Set.size updatedSet |> shouldBe 1
      Set.contains 10 updatedSet |> shouldBe True

    it "removes an element from a set" \_ -> do
      let initialSet = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let updatedSet = Set.remove 3 initialSet
      Set.size updatedSet |> shouldBe 3
      Set.contains 3 updatedSet |> shouldBe False
      Set.contains 1 updatedSet |> shouldBe True
      Set.contains 2 updatedSet |> shouldBe True
      Set.contains 4 updatedSet |> shouldBe True

    it "removing non-existent element doesn't change set" \_ -> do
      let initialSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let updatedSet = Set.remove 5 initialSet
      Set.size updatedSet |> shouldBe 3
      updatedSet |> shouldBe initialSet

    it "removes element from singleton set" \_ -> do
      let singleSet = Set.singleton (5 :: Int)
      let emptySet = Set.remove 5 singleSet
      Set.isEmpty emptySet |> shouldBe True

    it "unions two sets" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3] :: Set Int
      let set2 = Set.fromLinkedList [3, 4, 5] :: Set Int
      let unionSet = Set.union set2 set1
      Set.size unionSet |> shouldBe 5
      Set.contains 1 unionSet |> shouldBe True
      Set.contains 2 unionSet |> shouldBe True
      Set.contains 3 unionSet |> shouldBe True
      Set.contains 4 unionSet |> shouldBe True
      Set.contains 5 unionSet |> shouldBe True

    it "union with empty set returns the original set" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3] :: Set Int
      let emptySet = Set.empty :: Set Int
      let unionSet = Set.union emptySet set1
      unionSet |> shouldBe set1

    it "finds intersection of two sets" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let set2 = Set.fromLinkedList [3, 4, 5, 6] :: Set Int
      let intersectionSet = Set.intersection set2 set1
      Set.size intersectionSet |> shouldBe 2
      Set.contains 3 intersectionSet |> shouldBe True
      Set.contains 4 intersectionSet |> shouldBe True
      Set.contains 1 intersectionSet |> shouldBe False
      Set.contains 5 intersectionSet |> shouldBe False

    it "intersection with empty set returns empty set" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3] :: Set Int
      let emptySet = Set.empty :: Set Int
      let intersectionSet = Set.intersection emptySet set1
      Set.isEmpty intersectionSet |> shouldBe True

    it "finds difference of two sets" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let set2 = Set.fromLinkedList [3, 4, 5, 6] :: Set Int
      let differenceSet = Set.difference set2 set1
      Set.size differenceSet |> shouldBe 2
      Set.contains 1 differenceSet |> shouldBe True
      Set.contains 2 differenceSet |> shouldBe True
      Set.contains 3 differenceSet |> shouldBe False
      Set.contains 4 differenceSet |> shouldBe False

    it "difference with empty set returns the original set" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3] :: Set Int
      let emptySet = Set.empty :: Set Int
      let differenceSet = Set.difference emptySet set1
      differenceSet |> shouldBe set1

  describe "Set conversion operations" do
    it "converts set to LinkedList in sorted order" \_ -> do
      let testSet = Set.fromLinkedList [3, 1, 4, 1, 5, 9, 2, 6] :: Set Int
      let linkedList = Set.toLinkedList testSet
      linkedList |> shouldBe [1, 2, 3, 4, 5, 6, 9]

    it "converts empty set to empty LinkedList" \_ -> do
      let emptySet = Set.empty :: Set Int
      let linkedList = Set.toLinkedList emptySet
      linkedList |> shouldBe ([] :: LinkedList Int)

    it "converts set to Array in sorted order" \_ -> do
      let testSet = Set.fromLinkedList [3, 1, 4, 1, 5, 9, 2, 6] :: Set Int
      let array = Set.toArray testSet
      array |> shouldBe (Array.fromLinkedList [1, 2, 3, 4, 5, 6, 9])

    it "converts empty set to empty Array" \_ -> do
      let emptySet = Set.empty :: Set Int
      let array = Set.toArray emptySet
      array |> shouldBe (Array.empty :: Array Int)

    it "round-trip LinkedList -> Set -> LinkedList removes duplicates and sorts" \_ -> do
      let originalList = [5, 2, 8, 2, 1, 5] :: LinkedList Int
      let resultList = originalList |> Set.fromLinkedList |> Set.toLinkedList
      resultList |> shouldBe [1, 2, 5, 8]

    it "round-trip Array -> Set -> Array removes duplicates and sorts" \_ -> do
      let originalArray = Array.fromLinkedList [5, 2, 8, 2, 1, 5] :: Array Int
      let resultArray = originalArray |> Set.fromArray |> Set.toArray
      resultArray |> shouldBe (Array.fromLinkedList [1, 2, 5, 8])

  describe "Set transform operations" do
    it "maps a function over a set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let mappedSet = Set.map (\x -> x * 2) testSet
      Set.size mappedSet |> shouldBe 3
      Set.contains 2 mappedSet |> shouldBe True
      Set.contains 4 mappedSet |> shouldBe True
      Set.contains 6 mappedSet |> shouldBe True

    it "mapping can reduce set size if function produces duplicates" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let mappedSet = Set.map (\x -> x // 2) testSet
      -- 1->0, 2->1, 3->1, 4->2, so we get [0, 1, 2]
      Set.size mappedSet |> shouldBe 3
      Set.contains 0 mappedSet |> shouldBe True
      Set.contains 1 mappedSet |> shouldBe True
      Set.contains 2 mappedSet |> shouldBe True

    it "maps over empty set returns empty set" \_ -> do
      let emptySet = Set.empty :: Set Int
      let mappedSet = Set.map (\x -> x * 2) emptySet
      Set.isEmpty mappedSet |> shouldBe True

    it "keeps elements that pass the test (takeIf)" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3, 4, 5, 6] :: Set Int
      let filteredSet = Set.takeIf isEven testSet
      Set.size filteredSet |> shouldBe 3
      Set.contains 2 filteredSet |> shouldBe True
      Set.contains 4 filteredSet |> shouldBe True
      Set.contains 6 filteredSet |> shouldBe True
      Set.contains 1 filteredSet |> shouldBe False

    it "takeIf with always False returns empty set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let filteredSet = Set.takeIf (\_ -> False) testSet
      Set.isEmpty filteredSet |> shouldBe True

    it "takeIf with always True returns same set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let filteredSet = Set.takeIf (\_ -> True) testSet
      filteredSet |> shouldBe testSet

    it "drops elements that pass the test (dropIf)" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3, 4, 5, 6] :: Set Int
      let filteredSet = Set.dropIf isEven testSet
      Set.size filteredSet |> shouldBe 3
      Set.contains 1 filteredSet |> shouldBe True
      Set.contains 3 filteredSet |> shouldBe True
      Set.contains 5 filteredSet |> shouldBe True
      Set.contains 2 filteredSet |> shouldBe False

    it "dropIf with always False returns same set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let filteredSet = Set.dropIf (\_ -> False) testSet
      filteredSet |> shouldBe testSet

    it "dropIf with always True returns empty set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let filteredSet = Set.dropIf (\_ -> True) testSet
      Set.isEmpty filteredSet |> shouldBe True

    it "reduces a set from the right" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let sum = Set.reduce (+) 0 testSet
      sum |> shouldBe 10

    it "reduces with string concatenation" \_ -> do
      let testSet = Set.fromLinkedList ["a", "b", "c"]
      let concatenated = Set.reduce (\x acc -> x ++ acc) "" testSet
      -- Order depends on Set ordering, should produce a string with all elements
      Text.length concatenated |> shouldBe 3

    it "reduces empty set returns initial value" \_ -> do
      let emptySet = Set.empty :: Set Int
      let result = Set.reduce (+) 42 emptySet
      result |> shouldBe 42

    it "folds a set from the left" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let sum = Set.foldl (+) 0 testSet
      sum |> shouldBe 10

    it "foldl empty set returns initial value" \_ -> do
      let emptySet = Set.empty :: Set Int
      let result = Set.foldl (+) 100 emptySet
      result |> shouldBe 100

  describe "Set set operations properties" do
    it "union is commutative" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3] :: Set Int
      let set2 = Set.fromLinkedList [3, 4, 5] :: Set Int
      let union1 = Set.union set2 set1
      let union2 = Set.union set1 set2
      union1 |> shouldBe union2

    it "union is associative" \_ -> do
      let set1 = Set.fromLinkedList [1, 2] :: Set Int
      let set2 = Set.fromLinkedList [3, 4] :: Set Int
      let set3 = Set.fromLinkedList [5, 6] :: Set Int
      let leftAssoc = Set.union set3 (Set.union set2 set1)
      let rightAssoc = Set.union (Set.union set3 set2) set1
      leftAssoc |> shouldBe rightAssoc

    it "intersection is commutative" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let set2 = Set.fromLinkedList [3, 4, 5, 6] :: Set Int
      let inter1 = Set.intersection set2 set1
      let inter2 = Set.intersection set1 set2
      inter1 |> shouldBe inter2

    it "intersection is associative" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3, 4] :: Set Int
      let set2 = Set.fromLinkedList [2, 3, 4, 5] :: Set Int
      let set3 = Set.fromLinkedList [3, 4, 5, 6] :: Set Int
      let leftAssoc = Set.intersection set3 (Set.intersection set2 set1)
      let rightAssoc = Set.intersection (Set.intersection set3 set2) set1
      leftAssoc |> shouldBe rightAssoc

    it "difference is NOT commutative" \_ -> do
      let set1 = Set.fromLinkedList [1, 2, 3] :: Set Int
      let set2 = Set.fromLinkedList [2, 3, 4] :: Set Int
      let diff1 = Set.difference set2 set1
      let diff2 = Set.difference set1 set2
      diff1 |> shouldNotBe diff2

    it "union with itself returns same set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let unionSet = Set.union testSet testSet
      unionSet |> shouldBe testSet

    it "intersection with itself returns same set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let interSet = Set.intersection testSet testSet
      interSet |> shouldBe testSet

    it "difference with itself returns empty set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let diffSet = Set.difference testSet testSet
      Set.isEmpty diffSet |> shouldBe True

  describe "Set edge cases" do
    it "handles single element operations" \_ -> do
      let singleSet = Set.singleton (1 :: Int)
      Set.size singleSet |> shouldBe 1
      Set.contains 1 singleSet |> shouldBe True
      Set.isEmpty singleSet |> shouldBe False

    it "handles large sets" \_ -> do
      let largeList = [1 .. 1000] :: LinkedList Int
      let largeSet = Set.fromLinkedList largeList
      Set.size largeSet |> shouldBe 1000
      Set.contains 1 largeSet |> shouldBe True
      Set.contains 500 largeSet |> shouldBe True
      Set.contains 1000 largeSet |> shouldBe True
      Set.contains 1001 largeSet |> shouldBe False

    it "handles inserting and removing multiple times" \_ -> do
      let initialSet = Set.empty :: Set Int
      let set1 = Set.insert 1 initialSet
      let set2 = Set.insert 2 set1
      let set3 = Set.insert 3 set2
      let set4 = Set.remove 2 set3
      Set.size set4 |> shouldBe 2
      Set.contains 1 set4 |> shouldBe True
      Set.contains 2 set4 |> shouldBe False
      Set.contains 3 set4 |> shouldBe True

    it "handles Text elements" \_ -> do
      let textSet = Set.fromLinkedList (["hello", "world", "hello"] :: LinkedList Text)
      Set.size textSet |> shouldBe 2
      Set.contains "hello" textSet |> shouldBe True
      Set.contains "world" textSet |> shouldBe True
      Set.contains "foo" textSet |> shouldBe False

  describe "Set compatibility operations" do
    it "unwraps to Data.Set" \_ -> do
      let testSet = Set.fromLinkedList [1, 2, 3] :: Set Int
      let unwrapped = Set.unwrap testSet
      -- Verify it's a valid Data.Set (basic smoke test)
      unwrapped |> shouldNotBe (Set.unwrap (Set.empty :: Set Int))

    it "converts from legacy Data.Set" \_ -> do
      let legacySet = Data.Set.fromList [1, 2, 3] :: Data.Set.Set Int
      let neoSet = Set.fromLegacy legacySet
      Set.size neoSet |> shouldBe 3
      Set.contains 1 neoSet |> shouldBe True
      Set.contains 2 neoSet |> shouldBe True
      Set.contains 3 neoSet |> shouldBe True


