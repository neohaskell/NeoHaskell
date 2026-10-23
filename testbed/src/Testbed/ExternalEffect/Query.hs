{-# LANGUAGE TemplateHaskell #-}

module Testbed.ExternalEffect.Query (
  ExternalEffectSummary (..),
  canAccess,
  canView,
) where

import Core
import Json qualified
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Testbed.ExternalEffect.Core


data ExternalEffectSummary = ExternalEffectSummary
  { effectId :: Uuid,
    description :: Text,
    amount :: Int,
    status :: Text,
    externalId :: Maybe Text
  }
  deriving (Eq, Generic, Show)


instance Json.FromJSON ExternalEffectSummary


instance Json.ToJSON ExternalEffectSummary


instance ToSchema ExternalEffectSummary


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.publicAccess


canView :: Maybe UserClaims -> ExternalEffectSummary -> Maybe AccessError
canView = AccessControl.publicView


deriveQuery ''ExternalEffectSummary [''ExternalEffectEntity]


instance QueryOf ExternalEffectEntity ExternalEffectSummary where
  queryId = (.effectId)


  combine entity _maybeExisting = do
    let (status, externalId) = case entity.status of
          NotRequested -> ("not-requested", Nothing)
          Requested -> ("requested", Nothing)
          Delivered providerId -> ("delivered", Just providerId)
          Failed -> ("failed", Nothing)
    Update
      ExternalEffectSummary
        { effectId = entity.effectId,
          description = entity.description,
          amount = entity.amount,
          status = status,
          externalId = externalId
        }
