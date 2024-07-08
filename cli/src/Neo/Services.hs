module Neo.Services (Services, create, destroy) where

import Core
import Services qualified
import Services.EventStore (EventStore)


type Services =
  Services.Make
    '[ "unit" := Unit
     ]


create :: IO Services
create = do
  print "Creating services"
  let services =
        ANON
          { events = dieWith "hi" :: EventStore,
            unit = unit
          } ::
          Services
  pure services


destroy :: Services -> IO Unit
destroy _ = do
  print "Destroying services"
  pure unit