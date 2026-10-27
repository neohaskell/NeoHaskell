{-| Describe DNS lookup failures used by URL validation.
Use errorMessage when converting a failure to a validation message.
-}
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
