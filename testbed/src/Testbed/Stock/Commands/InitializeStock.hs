module Testbed.Stock.Commands.InitializeStock (
  InitializeStock (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Stock.Core


data InitializeStock = InitializeStock
  { productId :: Uuid
  , available :: Int
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON InitializeStock


getEntityId :: InitializeStock -> Maybe Uuid
getEntityId _ = Nothing


-- | Initialize stock for a product. Anonymous-friendly - ignores auth context.
decide :: InitializeStock -> Maybe StockEntity -> RequestContext -> Decision StockEvent
decide cmd entity _ctx = case entity of
  Just _ ->
    Decider.reject "Stock already initialized for this product!"
  Nothing ->
    if cmd.available < 0
      then Decider.reject "Available stock cannot be negative"
      else do
        id <- Decider.generateUuid
        Decider.acceptNew
          [ StockInitialized
              { entityId = id
              , productId = cmd.productId
              , available = cmd.available
              }
          ]


type instance EntityOf InitializeStock = StockEntity


type instance TransportOf InitializeStock = WebTransport


command ''InitializeStock
