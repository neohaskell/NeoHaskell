module Test.ServiceDefinitionSpec where

import Core
import Service.ServiceDefinition.Core qualified as ServiceDefinition
import Test


spec :: Spec Unit
spec = do
  describe "ServiceDefinition Monad behavior" do
    it "can build an empty service definition" \_ -> do
      let emptyServiceDefinition = ServiceDefinition.emptyServiceDefinition :: ServiceDefinition.ServiceDefinition '[] '[] '[] '[] Unit

      -- Should be able to extract the unit value
      ServiceDefinition.extract emptyServiceDefinition |> shouldBe unit

    it "can map over a service definition value" \_ -> do
      let serviceDef1 = ServiceDefinition.yield (5 :: Int)

      let serviceDef2 = ServiceDefinition.fmap (\x -> x * 2) serviceDef1

      ServiceDefinition.extract serviceDef2 |> shouldBe (10 :: Int)

    it "can use bind to compose service definitions" \_ -> do
      let serviceDef1 = ServiceDefinition.yield ("first" :: Text)

      let serviceDef2 = ServiceDefinition.yield ("second" :: Text)

      let composedServiceDef = serviceDef1 ServiceDefinition.>>= (\_ -> serviceDef2)

      ServiceDefinition.extract composedServiceDef |> shouldBe ("second" :: Text)

    it "can use pure to create a service definition" \_ -> do
      let serviceDef1 = ServiceDefinition.pure (42 :: Int)

      ServiceDefinition.extract serviceDef1 |> shouldBe (42 :: Int)
