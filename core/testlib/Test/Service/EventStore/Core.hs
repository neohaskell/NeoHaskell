module Test.Service.EventStore.Core (MyEvent (..)) where

import Core


data MyEvent
  = MyEvent
  deriving (Eq, Show, Ord, Generic)


instance Default MyEvent where
  def = MyEvent