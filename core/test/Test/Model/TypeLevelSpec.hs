module Test.Model.TypeLevelSpec where

import Core
import Data.Semigroup (Semigroup (..))
import Test


-- Test command types
data Command1 = Command1
  deriving (Generic, Typeable)


data Command2 = Command2
  deriving (Generic, Typeable)


data Command3 = Command3
  deriving (Generic, Typeable)


spec :: Spec Unit
spec = do
  describe "Model command tracking" do
    it "can create a model with no commands" \_ -> do
      let emptyModel :: Model '[] Unit
          emptyModel = yield unit

      extract emptyModel |> shouldBe unit
      isEmpty emptyModel |> shouldBe True

    it "can create a model with one command" \_ -> do
      let singleCommandModel :: Model '[] Unit
          singleCommandModel = do
            command @Command1
            yield unit

      extract singleCommandModel |> shouldBe unit
      hasCommand @Command1 singleCommandModel |> shouldBe True

    it "can create a model with multiple commands" \_ -> do
      let multiCommandModel :: Model '[] Unit
          multiCommandModel = do
            command @Command1
            command @Command2
            yield unit

      hasCommand @Command1 multiCommandModel |> shouldBe True
      hasCommand @Command2 multiCommandModel |> shouldBe True

    it "tracks commands added to model" \_ -> do
      let model :: Model '[] Unit
          model = do
            command @Command1
            command @Command2
            command @Command3
            yield unit

      hasCommand @Command1 model |> shouldBe True
      hasCommand @Command2 model |> shouldBe True
      hasCommand @Command3 model |> shouldBe True

    it "combining models merges command lists" \_ -> do
      let model1 :: Model '[] Unit
          model1 = do
            command @Command1
            yield unit

      let model2 :: Model '[] Unit
          model2 = do
            command @Command2
            yield unit

      let combined = model1 <> model2

      hasCommand @Command1 combined |> shouldBe True
      hasCommand @Command2 combined |> shouldBe True

    it "monadic composition preserves commands" \_ -> do
      let model1 :: Model '[] Text
          model1 = do
            _ <- command @Command1
            yield "result"

      let model2 :: Text -> Model '[] Text
          model2 prefix = do
            _ <- command @Command2
            yield (prefix ++ "-done")

      let composed = do
            result <- model1
            model2 result

      extract composed |> shouldBe "result-done"
      hasCommand @Command1 composed |> shouldBe True
      hasCommand @Command2 composed |> shouldBe True
