module Auth.UrlValidation.Dns.RecordType (
  DnsRecordType (..),
  hostArgument,
) where

import Text (Text)
import Basics


data DnsRecordType
  = ARecord
  | AAAARecord
  deriving (Eq, Show)


hostArgument :: DnsRecordType -> Text
hostArgument recordType = case recordType of
  ARecord -> "A"
  AAAARecord -> "AAAA"
