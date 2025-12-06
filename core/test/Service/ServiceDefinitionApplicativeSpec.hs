module Service.ServiceDefinitionApplicativeSpec (spec) where

import Core
import Test
import Prelude (id, (.), ($))
import Service.ServiceDefinition.Core qualified as SD

spec :: Spec Unit
spec = describe "ServiceDefinition Applicative Instance" do
  it "applies a function wrapped in ServiceDefinition to a value wrapped in ServiceDefinition" \_ -> do
    let fnDef = SD.pure (\x -> x + 1)
    let valDef = SD.pure (5 :: Int)
    let result = fnDef SD.<*> valDef
    SD.extract result `shouldBe` 6

  it "supports multiple applications" \_ -> do
    let addDef = SD.pure (+)
    let val1 = SD.pure (10 :: Int)
    let val2 = SD.pure (20 :: Int)
    let result = addDef SD.<*> val1 SD.<*> val2
    SD.extract result `shouldBe` 30

  it "supports function composition" \_ -> do
    let multiplyDef = SD.pure (\x -> x * 2)
    let addOneDef = SD.pure (\x -> x + 1)
    let composeDef = SD.pure (.) SD.<*> multiplyDef SD.<*> addOneDef
    let result = composeDef SD.<*> SD.pure (3 :: Int)
    SD.extract result `shouldBe` 8  -- (3 + 1) * 2 = 8

  it "follows applicative identity law: pure id <*> v = v" \_ -> do
    let value = SD.pure (42 :: Int)
    let result = SD.pure id SD.<*> value
    SD.extract result `shouldBe` SD.extract value

  it "follows applicative composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" \_ -> do
    let u = SD.pure (\x -> x * 2)
    let v = SD.pure (\x -> x + 10)
    let w = SD.pure (5 :: Int)

    let leftSide = SD.pure (.) SD.<*> u SD.<*> v SD.<*> w
    let rightSide = u SD.<*> (v SD.<*> w)

    SD.extract leftSide `shouldBe` SD.extract rightSide  -- Both should be (5 + 10) * 2 = 30

  it "follows applicative homomorphism law: pure f <*> pure x = pure (f x)" \_ -> do
    let f = (\x -> x * 3)
    let x = 7 :: Int

    let leftSide = SD.pure f SD.<*> SD.pure x
    let rightSide = SD.pure (f x)

    SD.extract leftSide `shouldBe` SD.extract rightSide  -- Both should be 21

  it "follows applicative interchange law: u <*> pure y = pure ($ y) <*> u" \_ -> do
    let u = SD.pure (\x -> x * 4)
    let y = 6 :: Int

    let leftSide = u SD.<*> SD.pure y
    let rightSide = SD.pure ($ y) SD.<*> u

    SD.extract leftSide `shouldBe` SD.extract rightSide  -- Both should be 24