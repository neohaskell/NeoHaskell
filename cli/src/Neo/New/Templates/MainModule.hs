module Neo.New.Templates.MainModule where

import Core


template :: Text -> Text
template moduleName = do
  [fmt|module #{moduleName} where

import Core


run :: Task Text ()
run = do
    print "Let's go NeoHaskell! ⏩"
  |]
