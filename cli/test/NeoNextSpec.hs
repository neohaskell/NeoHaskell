module NeoNextSpec where

import Core
import NeoNext
import Test


spec :: Spec Unit
spec = panic "not implemented yet"


appSpec :: AppSpec AppModel
appSpec = specificationFor appModel do
  scenario "neo shell runs a shell" do
    given receivedCommand ShellCommand
    expect registeredEvent ShellTriggered
    and executedTask InteractiveProcess
