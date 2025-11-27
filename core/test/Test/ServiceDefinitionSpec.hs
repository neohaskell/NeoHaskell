module Test.ServiceDefinitionSpec where

import Core
import Service.ServiceDefinition.Core qualified as ServiceDefinition
import Test


-- Test command types for service definition testing
data TestCommand1 = TestCommand1
  deriving (Generic, Typeable)


data TestCommand2 = TestCommand2
  deriving (Generic, Typeable)


spec :: Spec Unit
spec = do
  describe "ServiceDefinition Monad behavior" do
    it "can build an empty service definition" \_ -> do
      let emptyServiceDefinition :: ServiceDefinition '[] Unit
          emptyServiceDefinition = ServiceDefinition.yield unit

      -- Should be able to extract the unit value
      ServiceDefinition.extract emptyServiceDefinition |> shouldBe unit

    it "can map over a service definition value" \_ -> do
      let serviceDef1 :: ServiceDefinition '[] Int
          serviceDef1 = ServiceDefinition.yield 5

      let serviceDef2 = ServiceDefinition.fmap (\x -> x * 2) serviceDef1

      ServiceDefinition.extract serviceDef2 |> shouldBe 10

    it "can use bind to compose service definitions" \_ -> do
      let serviceDef1 :: ServiceDefinition '[] Text
          serviceDef1 = ServiceDefinition.yield "first"

      let serviceDef2 :: ServiceDefinition '[] Text
          serviceDef2 = ServiceDefinition.yield "second"

      let composedServiceDef = serviceDef1 ServiceDefinition.>>= (\_ -> serviceDef2)

      ServiceDefinition.extract composedServiceDef |> shouldBe "second"

    it "can use pure to create a service definition" \_ -> do
      let serviceDef1 :: ServiceDefinition '[] Int
          serviceDef1 = ServiceDefinition.pure 42

      ServiceDefinition.extract serviceDef1 |> shouldBe 42
