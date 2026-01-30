module Main (main) where

import Prelude (IO)

import Auth.InMemorySecretStoreSpec qualified
import Auth.JwtSpec qualified
import Auth.MiddlewareSpec qualified
import Auth.OAuth2.ClientSpec qualified
import Auth.OAuth2.RoutesSpec qualified
import Auth.OAuth2.StateTokenSpec qualified
import Auth.OAuth2.TokenRefreshSpec qualified
import Auth.OAuth2.TransactionStoreSpec qualified
import Auth.OAuth2.TypesSpec qualified
import Auth.UrlValidationSpec qualified
import Test.Hspec qualified as Hspec


main :: IO ()
main = Hspec.hspec do
  Hspec.describe "Auth.InMemorySecretStore" Auth.InMemorySecretStoreSpec.spec
  Hspec.describe "Auth.Jwt" Auth.JwtSpec.spec
  Hspec.describe "Auth.Middleware" Auth.MiddlewareSpec.spec
  Hspec.describe "Auth.OAuth2.Client" Auth.OAuth2.ClientSpec.spec
  Hspec.describe "Auth.OAuth2.Routes" Auth.OAuth2.RoutesSpec.spec
  Hspec.describe "Auth.OAuth2.StateToken" Auth.OAuth2.StateTokenSpec.spec
  Hspec.describe "Auth.OAuth2.TokenRefresh" Auth.OAuth2.TokenRefreshSpec.spec
  Hspec.describe "Auth.OAuth2.TransactionStore" Auth.OAuth2.TransactionStoreSpec.spec
  Hspec.describe "Auth.OAuth2.Types" Auth.OAuth2.TypesSpec.spec
  Hspec.describe "Auth.UrlValidation" Auth.UrlValidationSpec.spec
