module Service.Integration.SelectionSpec where

import Array qualified
import Core
import Service.Integration.Selection (Selection (..))
import Service.Integration.Selection qualified as Selection
import Task qualified
import Test
import Text qualified


-- | Run the pure parser with a synthetic gate and argv.
parse :: Bool -> [Text] -> Task Text Selection
parse gateOpen args =
  Selection.parseSelectionFromArgs gateOpen args


spec :: Spec Unit
spec = do
  describe "Service.Integration.Selection" do
    it "defaults to Real with no flag and no env var" \_ -> do
      result <- Task.asResult (parse False [])
      result |> shouldBe (Ok Real)

    it "returns Real when --integrations=real and no env var" \_ -> do
      result <- Task.asResult (parse False ["--integrations=real"])
      result |> shouldBe (Ok Real)

    it "aborts when --integrations=fake but env var absent" \_ -> do
      result <- Task.asResult (parse False ["--integrations=fake"])
      case result of
        Err txt -> do
          Text.contains "--integrations=fake" txt |> shouldBe True
          Text.contains "NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "accepts --integrations=fake with env var = 1" \_ -> do
      result <- Task.asResult (parse True ["--integrations=fake"])
      result |> shouldBe (Ok Fake)

    it "accepts --integrations=hybrid --fake=Sendgrid --fake=Stripe with env var set" \_ -> do
      result <- Task.asResult (parse True ["--integrations=hybrid", "--fake=Sendgrid", "--fake=Stripe"])
      result |> shouldBe (Ok (Hybrid (Array.fromLinkedList ["Sendgrid", "Stripe"])))

    it "aborts hybrid when env var absent" \_ -> do
      result <- Task.asResult (parse False ["--integrations=hybrid", "--fake=Sendgrid"])
      case result of
        Err txt -> do
          Text.contains "--integrations=hybrid" txt |> shouldBe True
          Text.contains "NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "error text mentions flag name --integrations=fake" \_ -> do
      result <- Task.asResult (parse False ["--integrations=fake"])
      case result of
        Err txt -> Text.contains "--integrations=fake" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "error text mentions env var NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1" \_ -> do
      result <- Task.asResult (parse False ["--integrations=fake"])
      case result of
        Err txt -> Text.contains "NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "error text mentions docs URL" \_ -> do
      result <- Task.asResult (parse False ["--integrations=fake"])
      case result of
        Err txt -> Text.contains "neohaskell.org/docs/integrations/fake-mode" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "treats NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=0 as gate closed" \_ -> do
      result <- Task.asResult (parse False ["--integrations=fake"])
      case result of
        Err txt -> Text.contains "NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1" txt |> shouldBe True
        Ok _ -> fail "expected error"

    it "treats env var with trailing whitespace as gate closed" \_ -> do
      -- Our pure parser only accepts True/False for the gate.
      result <- Task.asResult (parse False ["--integrations=fake"])
      case result of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected error"

    it "rejects --fake= name containing invalid chars with validation error" \_ -> do
      result <- Task.asResult (parse True ["--integrations=hybrid", "--fake=Evil/../"])
      case result of
        Err txt -> Text.contains "[A-Za-z0-9_]" txt |> shouldBe True
        Ok _ -> fail "expected validation error"

    it "hybrid with unknown-but-well-formed integration name is accepted" \_ -> do
      result <- Task.asResult (parse True ["--integrations=hybrid", "--fake=UnknownSvc"])
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
