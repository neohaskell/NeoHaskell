module Service.Core where

import Action (Action)
import Array (Array)
import Basics
import Text (Text)
import Trigger (Trigger)


type View = Text


data UserApp (model :: Type) (event :: Type) = UserApp
  { init :: ((model, Action event)),
    view :: (model -> View),
    triggers :: (Array (Trigger event)),
    update :: (event -> model -> (model, Action event))
  }
