module Test.ModelSpec where

import Core
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Integration.App.Cart.Commands.CreateCart (CreateCart)
import Integration.App.Cart.Core (CartEntity, CartEvent)
import Test


-- Test entity types for model testing
data TestEntity1 = TestEntity1
  deriving (Generic, Typeable)


data TestEvent1 = TestEvent1
  deriving (Generic, Typeable)


data TestCommand1 = TestCommand1
  deriving (Generic, Typeable)


data TestEntity2 = TestEntity2
  deriving (Generic, Typeable)


data TestEvent2 = TestEvent2
  deriving (Generic, Typeable)


data TestCommand2 = TestCommand2
  deriving (Generic, Typeable)


spec :: Spec Unit
spec = do
  describe "Model Monad behavior" do
    it "can build an empty model" \_ -> do
      let emptyModel :: Model '[] Unit
          emptyModel = yield unit

      -- Should be able to extract the unit value
      extract emptyModel |> shouldBe unit

    it "can define a model with a single entity" \_ -> do
      let singleEntityModel :: Model '[] Unit
          singleEntityModel = do
            entity @TestEntity1
            yield unit

      -- Model should successfully compile and return unit
      extract singleEntityModel |> shouldBe unit

    it "can define a model with entity and events" \_ -> do
      let entityEventsModel :: Model '[] Unit
          entityEventsModel = do
            entity @TestEntity1
            events @TestEvent1
            yield unit

      extract entityEventsModel |> shouldBe unit

    it "can define a model with entity, events, and command" \_ -> do
      let fullModel :: Model '[] Unit
          fullModel = do
            entity @TestEntity1
            events @TestEvent1
            command @TestCommand1
            yield unit

      extract fullModel |> shouldBe unit

    it "supports monadic composition with bind" \_ -> do
      let model1 :: Model '[] Text
          model1 = do
            entity @TestEntity1
            yield "first"

      let model2 :: Text -> Model '[] Text
          model2 prefix = do
            _ <- entity @TestEntity2
            yield (prefix ++ "-second")

      let composedModel = do
            result1 <- model1
            model2 result1

      extract composedModel |> shouldBe "first-second"

    it "preserves model definitions through monadic composition" \_ -> do
      let model1 :: Model '[] Unit
          model1 = do
            entity @TestEntity1
            events @TestEvent1

      let model2 :: Model '[] Unit
          model2 = do
            entity @TestEntity2
            events @TestEvent2

      let composedModel = do
            _ <- model1
            model2

      -- Both entities and events should be preserved
      hasEntity @TestEntity1 composedModel |> shouldBe True
      hasEntity @TestEntity2 composedModel |> shouldBe True
      hasEvents @TestEvent1 composedModel |> shouldBe True
      hasEvents @TestEvent2 composedModel |> shouldBe True

  describe "Model Monoid behavior" do
    it "has an identity element (mempty)" \_ -> do
      let emptyModel :: Model '[] Unit
          emptyModel = mempty

      extract emptyModel |> shouldBe unit
      isEmpty emptyModel |> shouldBe True

    it "combines two models with mappend" \_ -> do
      let model1 :: Model '[] Unit
          model1 = do
            entity @TestEntity1
            events @TestEvent1
            yield unit

      let model2 :: Model '[] Unit
          model2 = do
            entity @TestEntity2
            events @TestEvent2
            yield unit

      let combinedModel = model1 <> model2

      -- Both models' definitions should be present
      hasEntity @TestEntity1 combinedModel |> shouldBe True
      hasEntity @TestEntity2 combinedModel |> shouldBe True
      hasEvents @TestEvent1 combinedModel |> shouldBe True
      hasEvents @TestEvent2 combinedModel |> shouldBe True

    it "satisfies monoid left identity: mempty <> x = x" \_ -> do
      let model1 :: Model '[] Unit
          model1 = do
            entity @TestEntity1
            events @TestEvent1
            command @TestCommand1

      let leftIdentity = mempty <> model1

      leftIdentity |> shouldBe model1

    it "satisfies monoid right identity: x <> mempty = x" \_ -> do
      let model1 :: Model '[] Unit
          model1 = do
            entity @TestEntity1
            events @TestEvent1
            command @TestCommand1

      let rightIdentity = model1 <> mempty

      rightIdentity |> shouldBe model1

    it "satisfies monoid associativity: (x <> y) <> z = x <> (y <> z)" \_ -> do
      let model1 :: Model '[] Unit
          model1 = do
            entity @TestEntity1
            yield unit

      let model2 :: Model '[] Unit
          model2 = do
            events @TestEvent1
            yield unit

      let model3 :: Model '[] Unit
          model3 = do
            command @TestCommand1
            yield unit

      let leftAssoc = (model1 <> model2) <> model3
      let rightAssoc = model1 <> (model2 <> model3)

      leftAssoc |> shouldBe rightAssoc

    it "can combine multiple models into one" \_ -> do
      let cartModel :: Model '[] Unit
          cartModel = do
            entity @CartEntity
            events @CartEvent
            command @CreateCart

      let testModel1 :: Model '[] Unit
          testModel1 = do
            entity @TestEntity1
            events @TestEvent1

      let testModel2 :: Model '[] Unit
          testModel2 = do
            entity @TestEntity2
            events @TestEvent2

      let combinedModel = cartModel <> testModel1 <> testModel2

      -- All entities should be present
      hasEntity @CartEntity combinedModel |> shouldBe True
      hasEntity @TestEntity1 combinedModel |> shouldBe True
      hasEntity @TestEntity2 combinedModel |> shouldBe True

      -- All events should be present
      hasEvents @CartEvent combinedModel |> shouldBe True
      hasEvents @TestEvent1 combinedModel |> shouldBe True
      hasEvents @TestEvent2 combinedModel |> shouldBe True

      -- Command should be present
      hasCommand @CreateCart combinedModel |> shouldBe True
