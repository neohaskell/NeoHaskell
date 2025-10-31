module Test.AppSpec.AppSpec (
  AppSpec (..),
  AppSpecSteps (..),
  fromSteps,
  emptySteps,
) where

import Applicable (Applicative)
import Array qualified
import Core
import Mappable (Functor)
import Test.AppSpec.Scenario (Scenario)
import Thenable (Monad)


data AppSpec (appModel :: Type) = AppSpec
  { scenarios :: Array (Scenario appModel)
  }
  deriving (Show, Eq, Ord)


fromSteps :: AppSpecSteps appModel Unit -> AppSpec appModel
fromSteps steps =
  AppSpec
    { scenarios = steps.scenarios
    }


emptySteps :: AppSpecSteps appModel Unit
emptySteps =
  AppSpecSteps
    { scenarios = Array.empty
    }


data AppSpecSteps (appModel :: Type) (result :: Type) = AppSpecSteps
  { scenarios :: Array (Scenario appModel)
  }
  deriving (Eq, Show, Ord)


instance Functor (AppSpecSteps appModel) where
  fmap :: (a -> b) -> AppSpecSteps appModel a -> AppSpecSteps appModel b
  fmap = panic "fmap: not implemented"


instance Applicative (AppSpecSteps appModel) where
  pure :: a -> AppSpecSteps appModel a
  pure = panic "pure: not implemented"


  (<*>) :: AppSpecSteps appModel (a -> b) -> AppSpecSteps appModel a -> AppSpecSteps appModel b
  (<*>) = panic "<*>: not implemented"


instance Monad (AppSpecSteps appModel) where
  (>>=) :: AppSpecSteps appModel a -> (a -> AppSpecSteps appModel b) -> AppSpecSteps appModel b
  (>>=) = panic ">>=: not implemented"
