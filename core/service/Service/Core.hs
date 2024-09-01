module Service.Core where

import Action (Action)
import Array (Array)
import Basics
import Text (Text)
import Trigger (Trigger)


type View = Text


type UserApp (model :: Type) (event :: Type) =
  Record
    '[ "init" := ((model, Action event)),
       "view" := (model -> View),
       "triggers" := (Array (Trigger event)),
       "update" := (event -> model -> (model, Action event))
     ]
