module Test.AppSpec.Scenario (
  Scenario (..),
  ScenarioSteps (..),
  empty,
) where

import Applicable (Applicative)
import Core
import Mappable (Functor)
import Thenable (Monad)


data Scenario (appModel :: Type) = Scenario
  { name :: Text,
    steps :: ScenarioSteps appModel Unit
  }
  deriving (Eq, Show, Ord)


empty :: ScenarioSteps appModel Unit
empty = ScenarioSteps


data ScenarioSteps (appModel :: Type) (result :: Type) = ScenarioSteps
  deriving (Eq, Show, Ord)


instance Functor (ScenarioSteps appModel) where
  fmap :: (a -> b) -> ScenarioSteps appModel a -> ScenarioSteps appModel b
  fmap = panic "fmap: not implemented"


instance Applicative (ScenarioSteps appModel) where
  pure :: a -> ScenarioSteps appModel a
  pure = panic "pure: not implemented"


  (<*>) :: ScenarioSteps appModel (a -> b) -> ScenarioSteps appModel a -> ScenarioSteps appModel b
  (<*>) = panic "<*>: not implemented"


instance Monad (ScenarioSteps appModel) where
  (>>=) :: ScenarioSteps appModel a -> (a -> ScenarioSteps appModel b) -> ScenarioSteps appModel b
  (>>=) = panic ">>=: not implemented"
