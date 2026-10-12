module BytesSpec where

import Bytes qualified
import Core
import LinkedList qualified
import Result qualified
import Test
import Text qualified


spec :: Spec Unit
spec = parallel do
  describe "Bytes" do
    describe "getRandom" do
      it "generates the requested number of bytes" \_ -> do
        randomBytes <- Bytes.getRandom 32
        Bytes.length randomBytes |> shouldBe 32

      it "generates empty bytes for size zero" \_ -> do
        randomBytes <- Bytes.getRandom 0
        Bytes.length randomBytes |> shouldBe 0

      it "generates empty bytes for negative sizes" \_ -> do
        randomBytes <- Bytes.getRandom (-1)
        Bytes.length randomBytes |> shouldBe 0

      it "generates independent values" \_ -> do
        firstBytes <- Bytes.getRandom 32
        secondBytes <- Bytes.getRandom 32
        firstBytes |> shouldNotBe secondBytes

    describe "unpack" do
      it "unpack round-trips with pack" \_ -> do
        let bytes = [0, 1, 127, 128, 255]
        bytes |> Bytes.pack |> Bytes.unpack |> shouldBe bytes

        -- Agrees with the length the packed form reports.
        Bytes.pack bytes |> Bytes.unpack |> LinkedList.length |> shouldBe (Bytes.length (Bytes.pack bytes))

        -- The empty case is total, not an error.
        Bytes.empty |> Bytes.unpack |> shouldBe []

    describe "fromBase64" do
      it "round-trips every byte value through toBase64" \_ -> do
        let allBytes = Bytes.pack [0 .. 255]
        allBytes |> Bytes.toBase64 |> Bytes.fromBase64 |> shouldBe (Ok allBytes)

        -- Lengths 1, 2 and 3 cover both padding forms and no padding.
        let prefixes = [1, 2, 3] |> LinkedList.map (\size -> Bytes.take size allBytes)
        prefixes
          |> LinkedList.map (\prefix -> prefix |> Bytes.toBase64 |> Bytes.fromBase64)
          |> shouldBe (prefixes |> LinkedList.map Ok)

      it "round-trips random binary bytes through toBase64" \_ -> do
        randomBytes <- Bytes.getRandom 1021
        randomBytes |> Bytes.toBase64 |> Bytes.fromBase64 |> shouldBe (Ok randomBytes)

      it "decodes the RFC 4648 known answers including padding" \_ -> do
        let decodeText encoded = encoded |> Text.toBytes |> Bytes.fromBase64
        decodeText "Zg==" |> shouldBe (Ok (Text.toBytes "f"))
        decodeText "Zm8=" |> shouldBe (Ok (Text.toBytes "fo"))
        decodeText "Zm9v" |> shouldBe (Ok (Text.toBytes "foo"))
        decodeText "Zm9vYg==" |> shouldBe (Ok (Text.toBytes "foob"))
        decodeText "Zm9vYmE=" |> shouldBe (Ok (Text.toBytes "fooba"))
        decodeText "Zm9vYmFy" |> shouldBe (Ok (Text.toBytes "foobar"))

        -- NUL, 0xFF and 0xFE exercise the "+" and "/" alphabet characters.
        decodeText "AP/+" |> shouldBe (Ok (Bytes.pack [0x00, 0xFF, 0xFE]))

      it "decodes empty input to empty bytes" \_ -> do
        Bytes.empty |> Bytes.fromBase64 |> shouldBe (Ok Bytes.empty)

      it "rejects malformed Base64 instead of guessing" \_ -> do
        let decodeText encoded = encoded |> Text.toBytes |> Bytes.fromBase64
        -- Missing padding, extra padding, a character outside the alphabet,
        -- embedded whitespace, the URL-safe alphabet, a truncated quantum,
        -- and non-canonical trailing bits.
        decodeText "Zm9vYg" |> shouldSatisfy Result.isErr
        decodeText "Zg===" |> shouldSatisfy Result.isErr
        decodeText "Zm9v!g==" |> shouldSatisfy Result.isErr
        decodeText "Zm9v Yg==" |> shouldSatisfy Result.isErr
        decodeText "AP_-" |> shouldSatisfy Result.isErr
        decodeText "Z" |> shouldSatisfy Result.isErr
        decodeText "Zh==" |> shouldSatisfy Result.isErr
