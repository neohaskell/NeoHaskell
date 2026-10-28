-- | Save an IBAN on an existing cart through the Web command route.
module Testbed.Cart.Commands.SaveIBAN (
  SaveIBAN (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Cart.Core (CartEntity (..), CartEvent (..))


data SaveIBAN = SaveIBAN
  { cartId :: Uuid,
    iban :: Text
  }


getEntityId :: SaveIBAN -> Maybe Uuid
getEntityId cmd = Just cmd.cartId


decide :: SaveIBAN -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide cmd maybeCart _ctx = case maybeCart of
  Nothing -> Decider.reject "Cart not found!"
  Just cart ->
    Decider.acceptExisting
      [ IBANSaved
          { entityId = cart.cartId,
            iban = cmd.iban
          }
      ]


type instance EntityOf SaveIBAN = CartEntity


type instance TransportsOf SaveIBAN = '[WebTransport]


command ''SaveIBAN
