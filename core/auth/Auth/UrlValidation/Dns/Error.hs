module Auth.UrlValidation.Dns.Error (
  DnsError (..),
  errorMessage,
) where

import Basics
import Text (Text)


data DnsError
  = DnsSubprocessFailure Text
  | DnsNoAddressesResolved
  | DnsTaskFailure
  deriving (Eq, Show)


errorMessage :: DnsError -> Text
errorMessage dnsError = case dnsError of
  DnsSubprocessFailure message -> message
  DnsNoAddressesResolved -> "No IP addresses resolved"
  DnsTaskFailure -> "Internal error"
