module Neo.Services (Services, create, destroy) where

import Core
import Record qualified
import Services qualified
import Services.EventStore (EventStore)


type UserServices = '["unit" := Unit]


type Services =
  Services.Make UserServices


create :: IO Services
create = do
  print "Creating services"
  let defserv =
        ANON
          { events = dieWith "hi" :: EventStore
          }

  let userserv = ANON {unit = unit}
  let services = Record.merge defserv userserv
  pure services


destroy :: Services -> IO Unit
destroy _ = do
  print "Destroying services"
  pure unit