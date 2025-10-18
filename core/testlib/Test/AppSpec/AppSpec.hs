module Test.AppSpec.AppSpec (
  AppSpec (..),
) where

import Core
import Test.AppSpec.Scenario (Scenario)


data AppSpec (appModel :: Type) = AppSpec
  { scenarios :: Array (Scenario appModel)
  }
  deriving (Show, Eq, Ord)
