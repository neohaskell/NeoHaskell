module Test.ModelSpec where

import Core
import Service.Model.Core qualified as Model
import Test


-- Test command types for model testing
data TestCommand1 = TestCommand1
  deriving (Generic, Typeable)


data TestCommand2 = TestCommand2
  deriving (Generic, Typeable)


spec :: Spec Unit
spec = do
  describe "Model Monad behavior" do
    it "can build an empty model" \_ -> do
      let emptyModel :: Model '[] Unit
          emptyModel = Model.yield unit

      -- Should be able to extract the unit value
      Model.extract emptyModel |> shouldBe unit

    it "can map over a model value" \_ -> do
      let model1 :: Model '[] Int
          model1 = Model.yield 5

      let model2 = Model.fmap (\x -> x * 2) model1

      Model.extract model2 |> shouldBe 10

    it "can use bind to compose models" \_ -> do
      let model1 :: Model '[] Text
          model1 = Model.yield "first"

      let model2 :: Model '[] Text
          model2 = Model.yield "second"

      let composedModel = model1 Model.>>= (\_ -> model2)

      Model.extract composedModel |> shouldBe "second"

    it "can use pure to create a model" \_ -> do
      let model1 :: Model '[] Int
          model1 = Model.pure 42

      Model.extract model1 |> shouldBe 42
