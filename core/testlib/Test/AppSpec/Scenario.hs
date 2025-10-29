module Test.AppSpec.Scenario (
  Scenario (..),
  ScenarioDSL (..),
  empty,
) where

import Applicable (Applicative)
import Core
import Mappable (Functor)
import Thenable (Monad)


data Scenario (appModel :: Type) = Scenario
  { name :: Text,
    steps :: ScenarioDSL appModel Unit
  }
  deriving (Eq, Show, Ord)


empty :: ScenarioDSL appModel Unit
empty = panic "empty: not implemented"


data ScenarioDSL (appModel :: Type) (result :: Type) = ScenarioDSL
  { name :: Text
  }
  deriving (Eq, Show, Ord)


instance Functor (ScenarioDSL appModel) where
  fmap :: (a -> b) -> ScenarioDSL appModel a -> ScenarioDSL appModel b
  fmap = panic "fmap: not implemented"


instance Applicative (ScenarioDSL appModel) where
  pure :: a -> ScenarioDSL appModel a
  pure = panic "pure: not implemented"


  (<*>) :: ScenarioDSL appModel (a -> b) -> ScenarioDSL appModel a -> ScenarioDSL appModel b
  (<*>) = panic "<*>: not implemented"


instance Monad (ScenarioDSL appModel) where
  (>>=) :: ScenarioDSL appModel a -> (a -> ScenarioDSL appModel b) -> ScenarioDSL appModel b
  (>>=) = panic ">>=: not implemented"
