{-| Run the private DNS resolver specification.
Use this module as the nhcore-test-auth-dns test target entry point.
-}
module Main (main) where

import Auth.UrlValidation.DnsSpec qualified
import Prelude (IO)
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec Auth.UrlValidation.DnsSpec.spec
