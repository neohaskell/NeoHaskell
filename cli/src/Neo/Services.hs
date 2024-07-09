module Neo.Services (Services, create, destroy) where

import Core
import Neo.Event (Event)
import Record qualified
import Services qualified
import Services.EventStore.InMemory qualified as EventStore


type AppServiceFields = '["unit" := Unit]


type Services =
  Services.Make Event AppServiceFields


create :: IO Services
create = do
  print "Creating services"
  events <- EventStore.create
  let coreServices = ANON {events = events}
  let appServices = ANON {unit = unit}
  let services = Record.merge coreServices appServices
  pure services


destroy :: Services -> IO Unit
destroy _ = do
  print "Destroying services"
  pure unit