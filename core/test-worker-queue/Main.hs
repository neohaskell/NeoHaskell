-- | Run the integration worker queue behavior suite.
--
-- Cabal invokes 'main' to execute 'WorkerQueueControlSpec.spec'.
module Main (main) where

import Prelude (IO)
import Test.Hspec qualified as Hspec
import WorkerQueueControlSpec qualified


main :: IO ()
main = Hspec.hspec WorkerQueueControlSpec.spec
