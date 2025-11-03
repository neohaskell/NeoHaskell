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
    { scenarios = steps.stepScenarios
    }


emptySteps :: AppSpecSteps appModel Unit
emptySteps =
  AppSpecSteps
    { stepScenarios = Array.empty,
      result = unit
    }


data AppSpecSteps (appModel :: Type) (result :: Type) = AppSpecSteps
  { stepScenarios :: Array (Scenario appModel),
    result :: result
  }
  deriving (Eq, Show, Ord, Functor)


instance Applicative (AppSpecSteps appModel) where
  pure :: result -> AppSpecSteps appModel result
  pure result =
    emptySteps {result = result}


  (<*>) ::
    AppSpecSteps appModel (inputResult -> outputResult) ->
    AppSpecSteps appModel inputResult ->
    AppSpecSteps appModel outputResult
  (<*>) mapper valueCarrier = do
    let mapperFunction = mapper.result
    let value = valueCarrier.result
    AppSpecSteps
      { stepScenarios = mapper.stepScenarios |> Array.append valueCarrier.stepScenarios,
        result = mapperFunction value
      }


instance Monad (AppSpecSteps appModel) where
  (>>=) ::
    AppSpecSteps appModel inputResult ->
    (inputResult -> AppSpecSteps appModel outputResult) ->
    AppSpecSteps appModel outputResult
  (>>=) valueCarrier mapperFunction = do
    let value = valueCarrier.result
    let result = mapperFunction value
    let scenarios = valueCarrier.stepScenarios |> Array.append result.stepScenarios
    result
      { stepScenarios = scenarios
      }
