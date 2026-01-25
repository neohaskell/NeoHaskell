-- | Shared test utilities for SecretStore tests.
--
-- Provides helpers to create test TokenSets and TokenKeys
-- for use in SecretStore test suites.
module Test.Auth.TestUtils (
  -- * Test Data Builders
  makeTestTokenSet,
  makeTestTokenSetWithRefresh,
  makeTestTokenKey,
  makeTestTokenKeyWithPrefix,

  -- * Constants
  testAccessToken,
  testRefreshToken,
) where

import Auth.OAuth2.Types (
  AccessToken,
  RefreshToken,
  TokenSet (..),
  mkAccessToken,
  mkRefreshToken,
 )
import Auth.SecretStore (TokenKey (..))
import Basics
import Maybe (Maybe (..))
import Text (Text)


-- | A standard test access token value.
testAccessToken :: AccessToken
testAccessToken = mkAccessToken "test-access-token-12345"


-- | A standard test refresh token value.
testRefreshToken :: RefreshToken
testRefreshToken = mkRefreshToken "test-refresh-token-67890"


-- | Create a simple TokenSet for testing (no refresh token).
--
-- @
-- tokens <- makeTestTokenSet
-- store.put key tokens
-- @
makeTestTokenSet :: TokenSet
makeTestTokenSet =
  TokenSet
    { accessToken = testAccessToken
    , refreshToken = Nothing
    , expiresInSeconds = Just 3600
    }


-- | Create a TokenSet with a refresh token for testing.
--
-- @
-- tokens <- makeTestTokenSetWithRefresh
-- store.put key tokens
-- @
makeTestTokenSetWithRefresh :: TokenSet
makeTestTokenSetWithRefresh =
  TokenSet
    { accessToken = testAccessToken
    , refreshToken = Just testRefreshToken
    , expiresInSeconds = Just 3600
    }


-- | Create a TokenKey with a given identifier.
--
-- @
-- let key = makeTestTokenKey "user-123"
-- @
makeTestTokenKey :: Text -> TokenKey
makeTestTokenKey identifier = TokenKey identifier


-- | Create a TokenKey with a provider prefix.
--
-- @
-- let key = makeTestTokenKeyWithPrefix "oura" "user-123"
-- -- Results in: "oauth2:oura:user-123"
-- @
makeTestTokenKeyWithPrefix :: Text -> Text -> TokenKey
makeTestTokenKeyWithPrefix provider userId =
  TokenKey [fmt|oauth2:#{provider}:#{userId}|]
