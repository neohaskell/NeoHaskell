module Neo.Services (Services, create, destroy) where

import Core


type Services = Unit


create :: IO Services
create = do
  print "Creating services"
  pure unit


destroy :: Services -> IO Unit
destroy _ = do
  print "Destroying services"
  pure unit