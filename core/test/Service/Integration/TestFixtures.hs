{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Integration.TestFixtures
  ( SendEmail (..),
    SendEmailResponse (..),
    ChargeIntent (..),
    ChargeIntentResponse (..),
    mkSendEmail,
    mkChargeIntent,
  )
where

import Core
import Data.List qualified as GhcList
import Service.Integration.Adapter (Integration (..))
import Service.Integration.IntegrationError (IntegrationError (..))
import Task qualified
import Test.QuickCheck qualified as QuickCheck
import Text qualified


data SendEmail = SendEmail
  { to :: Text
  , templateId :: Text
  , variables :: [(Text, Text)]
  }
  deriving (Eq, Show, Generic)


instance QuickCheck.Arbitrary SendEmail where
  arbitrary = do
    to_ <- QuickCheck.arbitrary
    tid <- QuickCheck.arbitrary
    vars <- QuickCheck.arbitrary
    pure (SendEmail { to = Text.fromLinkedList to_, templateId = Text.fromLinkedList tid, variables = GhcList.map (\(k, v) -> (Text.fromLinkedList k, Text.fromLinkedList v)) vars })


data SendEmailResponse = SendEmailResponse
  { messageId :: Text
  , status :: Text
  }
  deriving (Eq, Show, Generic)


instance QuickCheck.Arbitrary SendEmailResponse where
  arbitrary = do
    mid <- QuickCheck.arbitrary
    st <- QuickCheck.arbitrary
    pure (SendEmailResponse { messageId = Text.fromLinkedList mid, status = Text.fromLinkedList st })


instance Integration SendEmail where
  type Response SendEmail = SendEmailResponse
  runReal _req = Task.throw (TransientFailure "not implemented in tests")
  runFake _req = do
    response <- QuickCheck.generate QuickCheck.arbitrary |> Task.fromIO
    Task.yield response


data ChargeIntent = ChargeIntent
  { intentId :: Text
  , amount :: Int
  }
  deriving (Eq, Show, Generic)


data ChargeIntentResponse = ChargeIntentResponse
  { status :: Text
  }
  deriving (Eq, Show, Generic)


instance Integration ChargeIntent where
  type Response ChargeIntent = ChargeIntentResponse
  runReal _req = Task.throw (TransientFailure "not implemented in tests")
  runFake _req = Task.yield (ChargeIntentResponse {status = "captured"})


mkSendEmail :: Text -> Text -> SendEmail
mkSendEmail to_ templateId_ = SendEmail { to = to_, templateId = templateId_, variables = [] }


mkChargeIntent :: Text -> Int -> ChargeIntent
mkChargeIntent intentId_ amount_ = ChargeIntent { intentId = intentId_, amount = amount_ }
