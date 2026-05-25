{-# OPTIONS_GHC -Wno-unused-imports #-}
module Service.Query.CheckpointSpec where

import Core
import Test.Spec (Spec, describe, it, pending)

spec :: Spec Unit
spec = do
  describe "CheckpointStore" do
    describe "getPositions" do
      it "H1: Two checkpoints stored returns Map with both entries" \_ ->
        pending "not implemented"

      it "E1: No checkpoints exist (fresh database) returns empty Map" \_ ->
        pending "not implemented"

      it "E2: 1000 checkpoints stored returns Map with 1000 entries, no truncation" \_ ->
        pending "not implemented"

      it "Er1: Postgres connection lost mid-query returns Text error" \_ ->
        pending "not implemented"

      it "P1: getPositions after setPosition returns the new position" \_ ->
        pending "not implemented"

    describe "setPosition" do
      it "H1: Set position for a new query inserts row and getPositions includes it" \_ ->
        pending "not implemented"

      it "E1: Overwrite existing checkpoint updates row and getPositions reflects new position" \_ ->
        pending "not implemented"

      it "E2: Set position to Int64 max value stored and retrievable" \_ ->
        pending "not implemented"

      it "Er1: Postgres connection lost mid-upsert returns Text error" \_ ->
        pending "not implemented"

      it "R1: setPosition then getPositions returns the set position (round-trip)" \_ ->
        pending "not implemented"

      it "P1: setPosition a 10 then setPosition a 20; getPosition a returns 20 (last-write-wins)" \_ ->
        pending "not implemented"

    describe "getMinPosition" do
      it "H1: Checkpoints at positions 5, 10, 15 returns Just (StreamPosition 5)" \_ ->
        pending "not implemented"

      it "E1: Single checkpoint at position 7 returns Just (StreamPosition 7)" \_ ->
        pending "not implemented"

      it "E2: No checkpoints returns Nothing" \_ ->
        pending "not implemented"

      it "Er1: Postgres connection lost returns Text error" \_ ->
        pending "not implemented"

  describe "needsProcessing" do
    it "H1: Event at position 5, query checkpoint at position 3 returns [queryName]" \_ ->
      pending "not implemented"

    it "E1: Event at position 5, query checkpoint at position 5 returns []" \_ ->
      pending "not implemented"

    it "E2: Event at position 5, query checkpoint at position 10 returns []" \_ ->
      pending "not implemented"

    it "E3: Event with no position metadata, query with checkpoint at 3 returns []" \_ ->
      pending "not implemented"

    it "Er1: Multiple queries, one behind and one caught up returns only the behind query's name" \_ ->
      pending "not implemented"

    it "P1: needsProcessing checkpoints event at P returns same result each call (deterministic)" \_ ->
      pending "not implemented"
