module NeoNext where

import Applicable (Applicative)
import Core
import Mappable (Functor)
import Thenable (Monad, Thenable)


data ShellCommand = ShellCommand


data ShellTriggered = ShellTriggered


-- | This should go into its own automation module
data InteractiveProcess = InteractiveProcess


data AppModel


appModel :: AppModel
appModel = panic "not implemented"


-- This should go into the Test module VVVVV
data AppSpec (appModel :: Type)


specificationFor ::
  appModel -> t appModel -> AppSpec appModel
specificationFor _ = panic "not implemented"


data ScenarioDef (appModel :: Type)
  deriving (Functor)


instance Applicative ScenarioDef


instance Monad ScenarioDef


scenario ::
  Text -> ScenarioDef appModel -> t appModel
scenario _ = panic "not implemented"


given ::
  adapter -> adapterInput -> ScenarioDef appModel
given = panic "not implemented"


expect ::
  adapter -> adapterInput -> ScenarioDef appModel
expect = panic "not implemented"


and ::
  adapter -> adapterInput -> ScenarioDef appModel
and = panic "not implemented"


receivedCommand :: adapter
receivedCommand = panic "not implemented"


registeredEvent :: adapter
registeredEvent = panic "not implemented"


executedTask :: adapter
executedTask = panic "not implemented"