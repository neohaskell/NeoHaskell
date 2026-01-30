module Main (main) where

import Prelude (IO)

import ConcurrentMapSpec qualified
import Http.ClientRawSpec qualified
import Http.ClientSpec qualified
import IntSpec qualified
import SchemaSpec qualified
import SetSpec qualified
import StreamSpec qualified
import Test.AppSpec.AppSpecSpec qualified
import Test.AppSpec.VerifySpec qualified
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "ConcurrentMap" ConcurrentMapSpec.spec
  Hspec.describe "Http.Client" Http.ClientSpec.spec
  Hspec.describe "Http.ClientRaw" Http.ClientRawSpec.spec
  Hspec.describe "Int" IntSpec.spec
  Hspec.describe "Schema" SchemaSpec.spec
  Hspec.describe "Set" SetSpec.spec
  Hspec.describe "Stream" StreamSpec.spec
  Hspec.describe "Test.AppSpec.AppSpec" Test.AppSpec.AppSpecSpec.spec
  Hspec.describe "Test.AppSpec.Verify" Test.AppSpec.VerifySpec.spec
