module Service.Integration.SelectionSpec where

import Array qualified
import Core
import Service.Integration.Selection (Selection (..))
import Service.Integration.Selection qualified as Selection
import Task qualified
import Test
import Text qualified


-- | Run the pure parser with a synthetic argv list.
parse :: [Text] -> Task Text Selection
parse args =
  Selection.parseSelectionFromArgs args


spec :: Spec Unit
spec = do
  describe "Service.Integration.Selection" do
    it "defaults to Real with no flag" \_ -> do
      result <- Task.asResult (parse [])
      result |> shouldBe (Ok Real)

    it "returns Real when --integrations=real" \_ -> do
      result <- Task.asResult (parse ["--integrations=real"])
      result |> shouldBe (Ok Real)

    it "accepts --integrations=fake" \_ -> do
      result <- Task.asResult (parse ["--integrations=fake"])
      result |> shouldBe (Ok Fake)

    it "accepts --integrations=hybrid --fake=Sendgrid --fake=Stripe" \_ -> do
      result <- Task.asResult (parse ["--integrations=hybrid", "--fake=Sendgrid", "--fake=Stripe"])
      result |> shouldBe (Ok (Hybrid (Array.fromLinkedList ["Sendgrid", "Stripe"])))

    it "rejects unknown --integrations=X value" \_ -> do
      result <- Task.asResult (parse ["--integrations=unknown"])
      case result of
        Err txt -> Text.contains "unknown" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "error text mentions the invalid value" \_ -> do
      result <- Task.asResult (parse ["--integrations=bogus"])
      case result of
        Err txt -> Text.contains "bogus" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "rejects --fake= name containing invalid chars" \_ -> do
      result <- Task.asResult (parse ["--integrations=hybrid", "--fake=Evil/../"])
      case result of
        Err txt -> Text.contains "[A-Za-z0-9_]" txt |> shouldBe True
        Ok _ -> fail "expected validation error"

    it "hybrid with unknown-but-well-formed integration name is accepted" \_ -> do
      result <- Task.asResult (parse ["--integrations=hybrid", "--fake=UnknownSvc"])
      result |> shouldBe (Ok (Hybrid (Array.fromLinkedList ["UnknownSvc"])))

    it "isFakeByName Sendgrid (Hybrid [Sendgrid, Stripe]) is True" \_ -> do
      let sel = Hybrid (Array.fromLinkedList ["Sendgrid", "Stripe"])
      Selection.isFakeByName "Sendgrid" sel |> shouldBe True

    it "isFakeByName Other (Hybrid [Sendgrid, Stripe]) is False" \_ -> do
      let sel = Hybrid (Array.fromLinkedList ["Sendgrid", "Stripe"])
      Selection.isFakeByName "Other" sel |> shouldBe False

    it "isFakeByName _ Fake always returns True" \_ -> do
      Selection.isFakeByName "Anything" Fake |> shouldBe True

    it "validateOrThrow Fake returns Fake" \_ -> do
      result <- Task.asResult (Selection.validateOrThrow Fake)
      result |> shouldBe (Ok Fake)
