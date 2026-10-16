{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Prelude (IO) -- HOOK-ALLOW: standalone script driver; no nhcore package build
import System.Exit qualified as Exit
import System.Process qualified as Process
import Test.Hspec qualified as Hspec

main :: IO ()
main = Hspec.hspec do
  Hspec.it "propagates an integration suite failure through test-all" do
    (status, _, _) <- Process.readProcessWithExitCode
      "python3"
      ["scripts/test-integrations-gate", "AllTestsRunner.test_integrations_failure_is_not_skipped"]
      ""
    Hspec.shouldBe status Exit.ExitSuccess
