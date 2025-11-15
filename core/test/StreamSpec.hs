module StreamSpec where

import Array qualified
import AsyncTask qualified
import Core
import Stream (StreamMessage (..))
import Stream qualified
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Stream" do
    describe "new" do
      it "creates a new stream" \_ -> do
        stream <- Stream.new @Int
        stream |> shouldSatisfy (\_ -> True)

    describe "writeItem and readNext" do
      it "writes and reads a single item" \_ -> do
        stream <- Stream.new @Text
        Stream.writeItem "hello" stream
        result <- Stream.readNext stream
        result |> shouldBe (Just "hello")

      it "writes and reads multiple items in order" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem 1 stream
        Stream.writeItem 2 stream
        Stream.writeItem 3 stream

        result1 <- Stream.readNext stream
        result2 <- Stream.readNext stream
        result3 <- Stream.readNext stream

        result1 |> shouldBe (Just 1)
        result2 |> shouldBe (Just 2)
        result3 |> shouldBe (Just 3)

      it "writes and reads different data types" \_ -> do
        stream <- Stream.new @Text
        Stream.writeItem ("text" :: Text) stream
        result <- Stream.readNext stream
        result |> shouldBe (Just ("text" :: Text))

    describe "end" do
      it "signals end of stream" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem (42 :: Int) stream
        Stream.end stream

        result1 <- Stream.readNext stream
        result2 <- Stream.readNext stream

        result1 |> shouldBe (Just (42 :: Int))
        result2 |> shouldBe Nothing

      it "returns nothing when stream is empty and ended" \_ -> do
        stream <- Stream.new @Int
        Stream.end stream
        result <- Stream.readNext stream
        result |> shouldBe Nothing

    describe "pushError" do
      it "throws error when error is written to stream" \_ -> do
        stream <- Stream.new @Int
        Stream.pushError "test error" stream

        taskResult <- Stream.readNext stream |> Task.asResult
        case taskResult of
          Ok _ -> fail "Expected error but got Ok"
          Err err -> err |> shouldBe "test error"

      it "throws error after reading normal items" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem (1 :: Int) stream
        Stream.writeItem (2 :: Int) stream
        Stream.pushError "error after items" stream

        result1 <- Stream.readNext stream
        result2 <- Stream.readNext stream
        result1 |> shouldBe (Just (1 :: Int))
        result2 |> shouldBe (Just (2 :: Int))

        taskResult <- Stream.readNext stream |> Task.asResult
        case taskResult of
          Ok _ -> fail "Expected error but got Ok"
          Err err -> err |> shouldBe "error after items"

    describe "fromArray" do
      it "creates stream from empty array" \_ -> do
        let arr = Array.empty @Int
        stream <- Stream.fromArray arr
        result <- Stream.toArray stream
        result |> shouldBe Array.empty

      it "creates stream from array with single item" \_ -> do
        let arr = Array.fromLinkedList [42 :: Int]
        stream <- Stream.fromArray arr
        result <- Stream.toArray stream
        result |> shouldBe arr

      it "creates stream from array with multiple items" \_ -> do
        let arr = Array.fromLinkedList [1, 2, 3, 4, 5 :: Int]
        stream <- Stream.fromArray arr
        result <- Stream.toArray stream
        result |> shouldBe arr

      it "creates stream from large array" \_ -> do
        let arr = Array.initialize 1000 identity
        stream <- Stream.fromArray arr
        result <- Stream.toArray stream
        result |> shouldBe arr

      it "can read items one by one from array stream" \_ -> do
        let arr = Array.fromLinkedList [10, 20, 30 :: Int]
        stream <- Stream.fromArray arr

        item1 <- Stream.readNext stream
        item2 <- Stream.readNext stream
        item3 <- Stream.readNext stream
        item4 <- Stream.readNext stream

        item1 |> shouldBe (Just 10)
        item2 |> shouldBe (Just 20)
        item3 |> shouldBe (Just 30)
        item4 |> shouldBe Nothing

    describe "toArray" do
      it "converts empty stream to empty array" \_ -> do
        stream <- Stream.new @Int
        Stream.end stream
        result <- Stream.toArray stream
        result |> shouldBe Array.empty

      it "converts stream with single item to array" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem 42 stream
        Stream.end stream
        result <- Stream.toArray stream
        result |> shouldBe (Array.fromLinkedList [42])

      it "converts stream with multiple items to array" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem 1 stream
        Stream.writeItem 2 stream
        Stream.writeItem 3 stream
        Stream.writeItem 4 stream
        Stream.writeItem 5 stream
        Stream.end stream

        result <- Stream.toArray stream
        result |> shouldBe (Array.fromLinkedList [1, 2, 3, 4, 5])

      it "converts stream with many items to array" \_ -> do
        stream <- Stream.new
        let items = Array.initialize 100 identity
        items |> Task.forEach (\item -> Stream.writeItem item stream)
        Stream.end stream

        result <- Stream.toArray stream
        result |> shouldBe items

      it "throws error when stream errors during conversion" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem (1 :: Int) stream
        Stream.pushError "conversion error" stream

        taskResult <- Stream.toArray stream |> Task.asResult
        case taskResult of
          Ok _ -> fail "Expected error but got Ok"
          Err err -> err |> shouldBe "conversion error"

    describe "consume" do
      it "consumes empty stream" \_ -> do
        stream <- Stream.new @Int
        Stream.end stream

        result <- Stream.consume (\accumulator item -> Task.yield (accumulator + item)) 0 stream
        result |> shouldBe 0

      it "sums numbers from stream" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem 1 stream
        Stream.writeItem 2 stream
        Stream.writeItem 3 stream
        Stream.writeItem 4 stream
        Stream.writeItem 5 stream
        Stream.end stream

        result <- Stream.consume (\accumulator item -> Task.yield (accumulator + item)) 0 stream
        result |> shouldBe 15

      it "concatenates strings from stream" \_ -> do
        stream <- Stream.new @Text
        Stream.writeItem "hello" stream
        Stream.writeItem " " stream
        Stream.writeItem "world" stream
        Stream.end stream

        result <- Stream.consume (\accumulator item -> Task.yield (accumulator ++ item)) "" stream
        result |> shouldBe "hello world"

      it "builds array from stream" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem 1 stream
        Stream.writeItem 2 stream
        Stream.writeItem 3 stream
        Stream.end stream

        result <-
          Stream.consume
            (\accumulator item -> Task.yield (Array.push item accumulator))
            Array.empty
            stream

        result |> shouldBe (Array.fromLinkedList [1, 2, 3])

      it "handles error during fold operation" \_ -> do
        stream <- Stream.new @Int
        Stream.writeItem 1 stream
        Stream.pushError "fold error" stream

        taskResult <-
          Stream.consume (\accumulator item -> Task.yield (accumulator + item)) 0 stream
            |> Task.asResult

        case taskResult of
          Ok _ -> fail "Expected error but got Ok"
          Err err -> err |> shouldBe "fold error"

    describe "concurrent operations" do
      it "handles concurrent writes and reads" \_ -> do
        stream <- Stream.new @Int

        let writer = do
              Stream.writeItem (1 :: Int) stream
              Stream.writeItem (2 :: Int) stream
              Stream.writeItem (3 :: Int) stream
              Stream.end stream

        let reader = do
              Stream.toArray stream

        writerTask <- AsyncTask.run writer
        result <- reader
        AsyncTask.waitFor writerTask

        result |> shouldBe (Array.fromLinkedList [1, 2, 3])

      it "handles multiple concurrent readers on different streams" \_ -> do
        stream1 <- Stream.new @Text
        stream2 <- Stream.new @Text

        Stream.writeItem "a" stream1
        Stream.writeItem "x" stream2
        Stream.end stream1
        Stream.end stream2

        task1 <- AsyncTask.run (Stream.toArray stream1)
        task2 <- AsyncTask.run (Stream.toArray stream2)

        result1 <- AsyncTask.waitFor task1
        result2 <- AsyncTask.waitFor task2

        result1 |> shouldBe (Array.fromLinkedList ["a"])
        result2 |> shouldBe (Array.fromLinkedList ["x"])

      it "handles slow producer with fast consumer" \_ -> do
        stream <- Stream.new @Int

        let producer = do
              Stream.writeItem 1 stream
              Stream.writeItem 2 stream
              Stream.writeItem 3 stream
              Stream.end stream

        let consumer = do
              Stream.toArray stream

        producerTask <- AsyncTask.run producer
        result <- consumer
        AsyncTask.waitFor producerTask

        result |> shouldBe (Array.fromLinkedList [1, 2, 3])

    describe "StreamMessage" do
      it "Item message wraps a value" \_ -> do
        let msg :: StreamMessage Int = Item 42
        case msg of
          Item value -> value |> shouldBe (42 :: Int)
          _ -> fail "Expected Item but got something else"

      it "EndOfStream message signals completion" \_ -> do
        let msg = EndOfStream @Int
        case msg of
          EndOfStream -> Task.yield unit
          _ -> fail "Expected EndOfStream but got something else"

      it "StreamError message contains error text" \_ -> do
        let msg = StreamError @Int "error message"
        case msg of
          StreamError err -> err |> shouldBe "error message"
          _ -> fail "Expected StreamError but got something else"

      it "StreamMessage supports equality" \_ -> do
        let msg1 = Item (42 :: Int)
        let msg2 = Item (42 :: Int)
        let msg3 = Item (43 :: Int)
        msg1 |> shouldBe msg2
        msg1 |> shouldNotBe msg3

    describe "edge cases" do
      it "handles empty stream followed by writes after read" \_ -> do
        stream <- Stream.new @Int

        let asyncWriter = do
              Stream.writeItem (99 :: Int) stream
              Stream.end stream

        writerTask <- AsyncTask.run asyncWriter
        result <- Stream.readNext stream
        AsyncTask.waitFor writerTask

        result |> shouldBe (Just (99 :: Int))

      it "handles stream with only errors" \_ -> do
        stream <- Stream.new @Int
        Stream.pushError "immediate error" stream

        taskResult <- Stream.readNext stream |> Task.asResult
        case taskResult of
          Ok _ -> fail "Expected error but got Ok"
          Err err -> err |> shouldBe "immediate error"

      it "handles large number of items" \_ -> do
        stream <- Stream.new @Int
        let count = 10000
        let items = Array.initialize count identity

        Task.forEach (\item -> Stream.writeItem item stream) items
        Stream.end stream

        result <- Stream.toArray stream
        Array.length result |> shouldBe count
        result |> shouldBe items
