{-# LANGUAGE TemplateHaskell #-}

module Testbed.AccountConnection.Query (
  AccountConnectionSummary (..),
  canAccess,
  canView,
) where

import Core
import Json qualified
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Testbed.AccountConnection.Core


data AccountConnectionSummary = AccountConnectionSummary
  { accountConnectionId :: Uuid,
    userId :: Text,
    status :: Text,
    tokenKeyMatches :: Bool,
    tokenStored :: Maybe Bool
  }
  deriving (Eq, Generic, Show)


instance Json.FromJSON AccountConnectionSummary


instance Json.ToJSON AccountConnectionSummary


instance ToSchema AccountConnectionSummary


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.authenticatedAccess


canView :: Maybe UserClaims -> AccountConnectionSummary -> Maybe AccessError
canView = AccessControl.ownerOnly (.userId)


deriveQuery ''AccountConnectionSummary [''AccountConnectionEntity]


instance QueryOf AccountConnectionEntity AccountConnectionSummary where
  queryId = (.accountConnectionId)


  combine entity _maybeExisting = do
    let (status, tokenKeyMatches, tokenStored) = case entity.state of
          NeverConnected -> ("not-connected", False, Nothing)
          Connected {tokenKeyMatches, tokenStored} -> ("connected", tokenKeyMatches, tokenStored)
          Disconnected {tokenStored} -> ("disconnected", False, tokenStored)
          ConnectionFailed -> ("failed", False, Nothing)
    Update
      AccountConnectionSummary
        { accountConnectionId = entity.accountConnectionId,
          userId = entity.userId,
          status = status,
          tokenKeyMatches = tokenKeyMatches,
          tokenStored = tokenStored
        }
