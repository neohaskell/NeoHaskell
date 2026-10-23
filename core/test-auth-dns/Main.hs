module Main (main) where

import Auth.UrlValidation.DnsSpec qualified
import Prelude (IO)
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec Auth.UrlValidation.DnsSpec.spec
