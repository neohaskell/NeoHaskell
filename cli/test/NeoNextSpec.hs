module NeoNextSpec where

import Core
import NeoNext
import Test


spec :: Spec Unit
spec = describe "Neo CLI" do
  it "has the app spec verified" \_ -> do
    verifyAppSpec appSpec


appSpec :: AppSpec AppModel
appSpec = specificationFor appModel do
  scenario "neo shell runs a shell" do
    given (receivedCommand ShellCommand)
    expect (registeredEvent ShellTriggered)
    and (executedTask InteractiveProcess)
