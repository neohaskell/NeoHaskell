{-# OPTIONS_GHC -Wno-unused-imports #-}
module Service.QueryObjectStore.PostgresSpec where

import Core
import Test.Spec (Spec, describe, it, pending)

spec :: Spec Unit
spec = do
  describe "Service.QueryObjectStore.Postgres" do
    describe "get" do
      it "returns Just query when instance exists" \_ -> do
        pending "not implemented"

      it "returns Nothing when instance does not exist" \_ -> do
        pending "not implemented"

      it "deserializes unicode JSON correctly" \_ -> do
        pending "not implemented"

      it "returns StorageError on connection failure" \_ -> do
        pending "not implemented"

    describe "atomicUpdate" do
      it "updates existing instance" \_ -> do
        pending "not implemented"

      it "creates new instance when Nothing → Just" \_ -> do
        pending "not implemented"

      it "deletes instance via Just → Nothing" \_ -> do
        pending "not implemented"

      it "returns SerializationError on invalid JSON" \_ -> do
        pending "not implemented"

      it "returns StorageError on connection failure" \_ -> do
        pending "not implemented"

      it "serializes concurrent updates without lost updates" \_ -> do
        pending "not implemented"

    describe "delete" do
      it "deletes existing instance" \_ -> do
        pending "not implemented"

      it "idempotent delete of nonexistent instance" \_ -> do
        pending "not implemented"

      it "returns StorageError on connection failure" \_ -> do
        pending "not implemented"

      it "recreate after delete restores the instance" \_ -> do
        pending "not implemented"

    describe "getAll" do
      it "returns Array with 3 instances" \_ -> do
        pending "not implemented"

      it "returns empty Array when no instances stored" \_ -> do
        pending "not implemented"

      it "returns Array with 10000 instances" \_ -> do
        pending "not implemented"

      it "returns StorageError on connection failure" \_ -> do
        pending "not implemented"
