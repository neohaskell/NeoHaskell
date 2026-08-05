{-# OPTIONS_GHC -Wno-unused-imports #-}

module ResultSpec where

import Core
import Data.Either qualified as Either
import Maybe (Maybe (..))
import Result (Result (..))
import Result qualified
import Test


spec :: Spec Unit
spec = do
  describe "Result" do
    describe "map" do
      it "maps over Ok" \_ -> do
        Result.map (\x -> (x :: Int) + 1) (Ok 1) |> shouldBe (Ok (2 :: Int) :: Result Text Int)

      it "does not map over Err" \_ -> do
        Result.map (\x -> (x :: Int) + 1) (Err "error") |> shouldBe (Err "error" :: Result Text Int)

    describe "andThen" do
      it "chains Ok" \_ -> do
        let toValidMonth month =
              if month >= (1 :: Int) && month <= (12 :: Int)
                then Ok month
                else Err ("months must be between 1 and 12" :: Text)
        Result.andThen toValidMonth (Ok (4 :: Int)) |> shouldBe (Ok 4 :: Result Text Int)

      it "fails on invalid Ok" \_ -> do
        let toValidMonth month =
              if month >= (1 :: Int) && month <= (12 :: Int)
                then Ok month
                else Err ("months must be between 1 and 12" :: Text)
        Result.andThen toValidMonth (Ok (0 :: Int)) |> shouldBe (Err "months must be between 1 and 12" :: Result Text Int)

      it "propagates Err" \_ -> do
        let toValidMonth month =
              if month >= (1 :: Int) && month <= (12 :: Int)
                then Ok month
                else Err ("months must be between 1 and 12" :: Text)
        Result.andThen toValidMonth (Err ("not a number" :: Text)) |> shouldBe (Err "not a number" :: Result Text Int)

    describe "withDefault" do
      it "returns Ok value" \_ -> do
        Result.withDefault (0 :: Int) (Ok (123 :: Int) :: Result Text Int) |> shouldBe (123 :: Int)

      it "returns default on Err" \_ -> do
        Result.withDefault (0 :: Int) (Err ("no" :: Text) :: Result Text Int) |> shouldBe (0 :: Int)

    describe "mapError" do
      it "does not map over Ok" \_ -> do
        Result.mapError (\x -> (x :: Int) + 1) (Ok 1) |> shouldBe (Ok 1 :: Result Int Int)

      it "maps over Err" \_ -> do
        Result.mapError (\x -> (x :: Int) + 1) (Err 1) |> shouldBe (Err 2 :: Result Int Int)

    describe "toMaybe" do
      it "converts Ok to Just" \_ -> do
        Result.toMaybe (Ok 1 :: Result Text Int) |> shouldBe (Just 1 :: Maybe Int)

      it "converts Err to Nothing" \_ -> do
        Result.toMaybe (Err "error" :: Result Text Int) |> shouldBe (Nothing :: Maybe Int)

    describe "fromMaybe" do
      it "converts Just to Ok" \_ -> do
        Result.fromMaybe ("error" :: Text) (Just (1 :: Int)) |> shouldBe (Ok 1 :: Result Text Int)

      it "converts Nothing to Err" \_ -> do
        Result.fromMaybe ("error" :: Text) (Nothing :: Maybe Int) |> shouldBe (Err "error" :: Result Text Int)

    describe "fromEither" do
      it "converts Right to Ok" \_ -> do
        Result.fromEither (Either.Right 42) |> shouldBe (Ok (42 :: Int) :: Result Text Int)

      it "converts Left to Err" \_ -> do
        Result.fromEither (Either.Left "nope") |> shouldBe (Err "nope" :: Result Text Int)

    describe "toEither" do
      it "converts Ok to Right" \_ -> do
        Result.toEither (Ok 42 :: Result Text Int) |> shouldBe (Either.Right 42 :: Either.Either Text Int)

      it "converts Err to Left" \_ -> do
        Result.toEither (Err "nope" :: Result Text Int) |> shouldBe (Either.Left "nope" :: Either.Either Text Int)

    describe "isOk" do
      it "returns True for Ok" \_ -> do
        Result.isOk (Ok 42 :: Result Text Int) |> shouldBe True

      it "returns False for Err" \_ -> do
        Result.isOk (Err "nope" :: Result Text Int) |> shouldBe False

    describe "isErr" do
      it "returns False for Ok" \_ -> do
        Result.isErr (Ok 42 :: Result Text Int) |> shouldBe False

      it "returns True for Err" \_ -> do
        Result.isErr (Err "nope" :: Result Text Int) |> shouldBe True
