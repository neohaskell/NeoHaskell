module Services (Make) where

import Core
import Services.EventStore (EventStore)


type CoreServicesFields =
  '["events" := EventStore]


type Make userServicesFields =
  Record (MergeFields CoreServicesFields userServicesFields)
