module Main (main) where

import Prelude (IO)

import Integration.CommandSpec qualified
import Integration.ContextSpec qualified
import Integration.DispatcherSpec qualified
import Integration.ExitSpec qualified
import Integration.RuntimeSpec qualified
import Integration.TimerSpec qualified
import Integration.RegistrySpec qualified
import IntegrationSpec qualified
import Service.Integration.InboundSpec qualified
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "Integration.Command" Integration.CommandSpec.spec
  Hspec.describe "Integration.Context" Integration.ContextSpec.spec
  Hspec.describe "Integration.Dispatcher" Integration.DispatcherSpec.spec
  Hspec.describe "Integration.Exit" Integration.ExitSpec.spec
  Hspec.describe "Integration.Runtime" Integration.RuntimeSpec.spec
  Hspec.describe "Integration.Timer" Integration.TimerSpec.spec
  Hspec.describe "Integration.Registry" Integration.RegistrySpec.spec
  Hspec.describe "Integration" IntegrationSpec.spec
  Hspec.describe "Service.Integration.Inbound" Service.Integration.InboundSpec.spec
