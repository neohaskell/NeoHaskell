module Services (Make) where

import Core
import Data.Kind (Type)
import Record qualified
import Services.EventStore (EventStore)


type CoreServicesFields events =
  '["events" := (EventStore events)]


type Make (events :: Type) (userServicesFields :: Record.Fields a) =
  Record.MergeFields
    (CoreServicesFields events)
    userServicesFields