module Neo.Services (Services, init) where

import Core


type Services = Unit


init :: IO Services
init = do
  print "Initializing services"
  pure unit