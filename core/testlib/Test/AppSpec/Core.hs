module Test.AppSpec.Core (
  AppSpec (..),
) where

import Core


data AppSpec (appModel :: Type) = AppSpec
  deriving (Show, Eq, Ord)
