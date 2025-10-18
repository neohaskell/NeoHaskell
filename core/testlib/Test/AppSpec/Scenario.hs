module Test.AppSpec.Scenario (
  Scenario,
  ScenarioDSL (..),
) where

import Applicable (Applicative)
import Core
import Mappable (Functor)
import Thenable (Monad)


type Scenario (appModel :: Type) =
  ScenarioDSL (appModel :: Type) Unit


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
