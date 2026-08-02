module BytesSpec where

import Bytes qualified
import Core
import Test


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
