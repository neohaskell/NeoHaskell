{-# OPTIONS_GHC -Wno-unused-imports #-}

module IOSpec where

import Core
import Control.Exception qualified as GhcException
import Data.Either (Either (..))
import IO qualified
import System.Exit (ExitCode (..))
import Test


spec :: Spec Unit
spec = do
  describe "IO" do
    describe "exitFailure" do
      it "exitFailure exits with the given code" \_ -> do
        let result = IO.dangerouslyRun (GhcException.try @ExitCode (IO.exitFailure 1))
        result `shouldBe` (Left (ExitFailure 1) :: Either ExitCode ())

      it "exitFailure with code 42" \_ -> do
        let result = IO.dangerouslyRun (GhcException.try @ExitCode (IO.exitFailure 42))
        result `shouldBe` (Left (ExitFailure 42) :: Either ExitCode ())

      it "exitFailure with code 255 (max typical code)" \_ -> do
        let result = IO.dangerouslyRun (GhcException.try @ExitCode (IO.exitFailure 255))
        result `shouldBe` (Left (ExitFailure 255) :: Either ExitCode ())
    describe "FilePath" do
      it "FilePath is a type alias for String" \_ -> do
        let fp = "/some/path" :: IO.FilePath
        fp `shouldBe` "/some/path"

      it "FilePath can hold absolute paths" \_ -> do
        let fp = "/usr/local/bin" :: IO.FilePath
        fp `shouldBe` "/usr/local/bin"

      it "FilePath can hold relative paths" \_ -> do
        let fp = "src/Main.hs" :: IO.FilePath
        fp `shouldBe` "src/Main.hs"
