{-# OPTIONS_GHC -Wno-unused-imports #-}

module EventVariantOfSpec where

import Core
import Array qualified
import Decider qualified
import EventVariantOf (EventVariantOf (..), event)
import EventVariantOf qualified
import Service.Event.StreamPosition (StreamPosition (..))
import Test
import Text (Text)


-- ============================================================
-- Test-only types (NOT exported, NOT in the library)
-- ============================================================

-- | Test event ADT with two constructors.
data TestEvent
  = TestEventA { testId :: Int }
  | TestEventB { testLabel :: Text }
  deriving (Eq, Show, Generic)


-- | Standalone event type that maps to TestEventA.
data StandaloneCreated = StandaloneCreated
  { standaloneId :: Int
  }
  deriving (Eq, Show, Generic)


-- | EventVariantOf instance bridging StandaloneCreated to TestEvent.
instance EventVariantOf TestEvent StandaloneCreated where
  fromVariant created = TestEventA { testId = created.standaloneId }


-- | Second standalone event type that maps to TestEventB.
data StandaloneLabeled = StandaloneLabeled
  { label :: Text
  }
  deriving (Eq, Show, Generic)


-- | EventVariantOf instance bridging StandaloneLabeled to TestEvent.
instance EventVariantOf TestEvent StandaloneLabeled where
  fromVariant labeled = TestEventB { testLabel = labeled.label }


-- ============================================================
-- Tests
-- ============================================================

spec :: Spec Unit
spec = do
  describe "EventVariantOf" do

    describe "identity instance" do
      -- Test 1: happy
      it "fromVariant returns the same value for the ADT type" \_ -> do
        let original = TestEventA { testId = 42 }
        let result = fromVariant original :: TestEvent
        result |> shouldBe (TestEventA { testId = 42 })

      -- Test 2: happy
      it "fromVariant preserves TestEventB constructor" \_ -> do
        let original = TestEventB { testLabel = "hello" }
        let result = fromVariant original :: TestEvent
        result |> shouldBe (TestEventB { testLabel = "hello" })

      -- Test 3: edge — zero boundary value
      it "fromVariant preserves zero boundary value" \_ -> do
        let original = TestEventA { testId = 0 }
        let result = fromVariant original :: TestEvent
        result |> shouldBe (TestEventA { testId = 0 })

      -- Test 4: edge — empty text
      it "fromVariant preserves empty text" \_ -> do
        let original = TestEventB { testLabel = "" }
        let result = fromVariant original :: TestEvent
        result |> shouldBe (TestEventB { testLabel = "" })

    describe "custom variant instance" do
      -- Test 5: happy
      it "fromVariant converts StandaloneCreated to TestEventA" \_ -> do
        let standalone = StandaloneCreated { standaloneId = 99 }
        let result = fromVariant standalone :: TestEvent
        result |> shouldBe (TestEventA { testId = 99 })

      -- Test 6: happy
      it "fromVariant converts StandaloneLabeled to TestEventB" \_ -> do
        let standalone = StandaloneLabeled { label = "world" }
        let result = fromVariant standalone :: TestEvent
        result |> shouldBe (TestEventB { testLabel = "world" })

      -- Test 7: edge — zero value
      it "fromVariant converts variant with zero value" \_ -> do
        let standalone = StandaloneCreated { standaloneId = 0 }
        let result = fromVariant standalone :: TestEvent
        result |> shouldBe (TestEventA { testId = 0 })

      -- Test 8: edge — negative value
      it "fromVariant converts variant with negative value" \_ -> do
        let standalone = StandaloneCreated { standaloneId = -1 }
        let result = fromVariant standalone :: TestEvent
        result |> shouldBe (TestEventA { testId = -1 })

    describe "event helper" do
      -- Test 9: happy
      it "event is an alias for fromVariant (identity)" \_ -> do
        let original = TestEventA { testId = 7 }
        let result = event original :: TestEvent
        result |> shouldBe (TestEventA { testId = 7 })

      -- Test 10: happy
      it "event converts standalone type to ADT" \_ -> do
        let standalone = StandaloneCreated { standaloneId = 55 }
        let result = event standalone :: TestEvent
        result |> shouldBe (TestEventA { testId = 55 })

      -- Test 11: edge — second standalone type
      it "event converts second standalone type" \_ -> do
        let standalone = StandaloneLabeled { label = "via-event" }
        let result = event standalone :: TestEvent
        result |> shouldBe (TestEventB { testLabel = "via-event" })

    describe "Decider smart constructors with identity instance" do
      -- Test 12: happy
      it "acceptNew works with ADT constructors directly" \_ -> do
        let decision = Decider.acceptNew [TestEventA { testId = 1 }] :: Decision TestEvent
        -- The decision should be constructable without error.
        -- We verify it produces the Accept GADT constructor by pattern matching.
        case decision of
          Decider.Accept _ events ->
            Array.length events |> shouldBe 1
          _ ->
            fail "Expected Accept constructor"

      -- Test 13: happy
      it "acceptExisting works with ADT constructors directly" \_ -> do
        let decision = Decider.acceptExisting [TestEventB { testLabel = "test" }] :: Decision TestEvent
        case decision of
          Decider.Accept _ events ->
            Array.length events |> shouldBe 1
          _ ->
            fail "Expected Accept constructor"

      -- Test 14: edge — multiple identity events
      it "acceptAny handles multiple identity events" \_ -> do
        let decision =
              Decider.acceptAny
                [ TestEventA { testId = 1 }
                , TestEventA { testId = 2 }
                , TestEventB { testLabel = "three" }
                ] :: Decision TestEvent
        case decision of
          Decider.Accept _ events ->
            Array.length events |> shouldBe 3
          _ ->
            fail "Expected Accept constructor"

    describe "Decider smart constructors with custom variant" do
      -- Test 15: happy
      it "acceptNew converts standalone variant to ADT" \_ -> do
        let decision = Decider.acceptNew [StandaloneCreated { standaloneId = 10 }] :: Decision TestEvent
        case decision of
          Decider.Accept _ events -> do
            Array.length events |> shouldBe 1
            let firstEvent = events |> Array.first
            firstEvent |> shouldBe (Just (TestEventA { testId = 10 }))
          _ ->
            fail "Expected Accept constructor"

      -- Test 16: happy
      it "acceptExisting converts standalone variant to ADT" \_ -> do
        let decision = Decider.acceptExisting [StandaloneLabeled { label = "converted" }] :: Decision TestEvent
        case decision of
          Decider.Accept _ events -> do
            Array.length events |> shouldBe 1
            let firstEvent = events |> Array.first
            firstEvent |> shouldBe (Just (TestEventB { testLabel = "converted" }))
          _ ->
            fail "Expected Accept constructor"

      -- Test 17: edge — acceptAfter with position
      it "acceptAfter handles variant events with position" \_ -> do
        let decision =
              Decider.acceptAfter (StreamPosition 5)
                [ StandaloneCreated { standaloneId = 100 }
                ] :: Decision TestEvent
        case decision of
          Decider.Accept _ events -> do
            Array.length events |> shouldBe 1
            let firstEvent = events |> Array.first
            firstEvent |> shouldBe (Just (TestEventA { testId = 100 }))
          _ ->
            fail "Expected Accept constructor"

    describe "mixed event types via event helper" do
      -- Test 18: happy
      it "event helper enables heterogeneous event arrays" \_ -> do
        let decision =
              Decider.acceptExisting
                [ event (StandaloneCreated { standaloneId = 1 }) :: TestEvent
                , event (StandaloneLabeled { label = "mixed" }) :: TestEvent
                ] :: Decision TestEvent
        case decision of
          Decider.Accept _ events -> do
            Array.length events |> shouldBe 2
            let first = events |> Array.first
            let second = events |> Array.get 1
            first |> shouldBe (Just (TestEventA { testId = 1 }))
            second |> shouldBe (Just (TestEventB { testLabel = "mixed" }))
          _ ->
            fail "Expected Accept constructor"

      -- Test 19: edge — mixed identity and variant via event
      it "acceptNew with mixed identity and variant via event" \_ -> do
        let decision =
              Decider.acceptNew
                [ event (TestEventA { testId = 5 }) :: TestEvent
                , event (StandaloneCreated { standaloneId = 6 }) :: TestEvent
                ] :: Decision TestEvent
        case decision of
          Decider.Accept _ events -> do
            Array.length events |> shouldBe 2
            let first = events |> Array.first
            let second = events |> Array.get 1
            first |> shouldBe (Just (TestEventA { testId = 5 }))
            second |> shouldBe (Just (TestEventA { testId = 6 }))
          _ ->
            fail "Expected Accept constructor"
