{-# LANGUAGE TypeFamilies #-}

module Collection (
  Collection (..),
  length,
) where

import Basics hiding (Item)
import Data.Vector qualified
import IO (IO)
import LinkedList (LinkedList)
import Maybe (Maybe (..))


-- | The Collection typeclass defines the behavior of a sequence whose
-- elements can be traversed and accessed by an indexed subscript.
class Collection (f :: Type -> Type) where
  emptyImpl :: forall a. f a
  repeatImpl :: Int -> a -> f a
  wrapImpl :: a -> f a
  fromLinkedListImpl :: LinkedList a -> f a


  isEmptyImpl :: f a -> Bool
  lengthImpl :: f a -> Int
  indicesImpl :: f a -> [Int]
  getImpl :: Int -> f a -> Maybe a
  firstImpl :: f a -> Maybe a


  setImpl :: Int -> a -> f a -> f a
  pushImpl :: a -> f a -> f a
  appendImpl :: f a -> f a -> f a
  sliceImpl :: Int -> Int -> f a -> f a


  toLinkedListImpl :: f a -> LinkedList a
  toIndexedLinkedListImpl :: f a -> LinkedList (Int, a)


  mapImpl :: (a -> b) -> f a -> f b
  indexedMapImpl :: (Int -> a -> b) -> f a -> f b
  reduceImpl :: (a -> b -> b) -> b -> f a -> b
  flatMapImpl :: (a -> f b) -> f a -> f b
  takeIfImpl :: (a -> Bool) -> f a -> f a
  dropIfImpl :: (a -> Bool) -> f a -> f a
  dropWhileImpl :: (a -> Bool) -> f a -> f a
  takeWhileImpl :: (a -> Bool) -> f a -> f a
  takeImpl :: Int -> f a -> f a
  dropImpl :: Int -> f a -> f a
  indexedImpl :: f a -> f (Int, a)
  foldlImpl :: (a -> b -> b) -> b -> f a -> b
  foldMImpl :: forall a b. (b -> a -> IO b) -> b -> f a -> IO b


  partitionByImpl :: (a -> Bool) -> f a -> (f a, f a)
  splitFirstImpl :: f a -> Maybe (a, f a)
  anyImpl :: (a -> Bool) -> f a -> Bool


  fromLegacyImpl :: Data.Vector.Vector a -> f a
  lastImpl :: f a -> Maybe a
  zipImpl :: f a -> f b -> f (b, a)
  sumIntegersImpl :: f Int -> Int
  reverseImpl :: f a -> f a


-- | Wrapper functions without the Impl suffix
length :: (Collection f) => f a -> Int
length = lengthImpl
