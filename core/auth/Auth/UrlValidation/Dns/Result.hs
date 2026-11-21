{-| Represent DNS lookup outcomes used by URL validation.
Use combine to merge A and AAAA results before validating addresses.
-}
module Auth.UrlValidation.Dns.Result (
  DnsResult (..),
  combine,
) where

import Appendable ((++))
import Auth.UrlValidation.Dns.Error (DnsError (..))
import Basics
import Text (Text)


data DnsResult
  = DnsResolved [Text]
  | DnsFailed DnsError
  | DnsTimedOut
  deriving (Eq, Show)


combine :: DnsResult -> DnsResult -> DnsResult
combine aResult aaaaResult = case (aResult, aaaaResult) of
  (DnsTimedOut, _) -> DnsTimedOut
  (_, DnsTimedOut) -> DnsTimedOut
  (DnsResolved aAddresses, DnsResolved aaaaAddresses) -> DnsResolved (aAddresses ++ aaaaAddresses)
  (DnsResolved addresses, DnsFailed _) -> DnsResolved addresses
  (DnsFailed _, DnsResolved addresses) -> DnsResolved addresses
  (DnsFailed aError, DnsFailed _) -> DnsFailed aError
