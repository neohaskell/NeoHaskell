{-# OPTIONS_GHC -Wno-unused-imports #-}

module ConsoleSpec where

import Core
import Console qualified
import Task qualified
import Test



spec :: Spec Unit
spec = do
  describe "Console" do
    describe "error" do
      it "writes to stderr without crashing" \_ -> do
        Task.fromIO (Console.error "test error message")

      it "accepts empty text" \_ -> do
        Task.fromIO (Console.error "")

      it "accepts multi-line text" \_ -> do
        Task.fromIO (Console.error "line 1\nline 2\nline 3")
