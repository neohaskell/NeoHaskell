module Auth.UrlValidation.DnsSpec where

import AsyncTask qualified
import Auth.UrlValidation.Dns (DnsActions (..), DnsConfig (..), DnsError (..), DnsRecordType (..), DnsResult (..), errorMessage, parseConfig, resolveWith)
import Auth.UrlValidation.Dns.RecordType qualified as RecordType
import Channel qualified
import Core
import Task qualified
import Test


spec :: Spec Unit
spec = parallel do
  describe "Auth.UrlValidation.Dns" do
    it "accepts bounded DNS timeouts with complete integer input" \_ -> do
      parseConfig "1" |> shouldBe (DnsConfig {timeoutSeconds = 1})
      parseConfig "60" |> shouldBe (DnsConfig {timeoutSeconds = 60})
      parseConfig " 7 " |> shouldBe (DnsConfig {timeoutSeconds = 7})

    it "uses the default for malformed or out-of-range DNS timeouts" \_ -> do
      let defaultConfig = DnsConfig {timeoutSeconds = 5}
      parseConfig "0" |> shouldBe defaultConfig
      parseConfig "61" |> shouldBe defaultConfig
      parseConfig "-1" |> shouldBe defaultConfig
      parseConfig "7x" |> shouldBe defaultConfig
      parseConfig "7 8" |> shouldBe defaultConfig
      parseConfig "invalid" |> shouldBe defaultConfig
      parseConfig "+7" |> shouldBe defaultConfig
      parseConfig "+ 7" |> shouldBe defaultConfig

    it "renders each record type only at the host argument boundary" \_ -> do
      RecordType.hostArgument ARecord |> shouldBe "A"
      RecordType.hostArgument AAAARecord |> shouldBe "AAAA"

    it "starts both address family lookups before awaiting either result" \_ -> do
      started <- Channel.newBounded 2 :: Task Text (Channel.Channel DnsRecordType)
      release <- Channel.newBounded 2 :: Task Text (Channel.Channel Unit)
      deadlineGate <- Channel.newBounded 1 :: Task Text (Channel.Channel Unit)
      let lookupRecord recordType _ = do
            Channel.write recordType started
            Channel.read release
            Task.yield (DnsResolved [])
      let actions = DnsActions
            { lookupDnsRecord = lookupRecord,
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      withResolver (DnsConfig {timeoutSeconds = 3}) actions "example.org" \resolver -> do
        firstFamily <- readWithin "first address family did not start" started
        secondFamily <- readWithin "second address family did not start" started
        assertBothFamilies firstFamily secondFamily
        Channel.write unit release
        Channel.write unit release
        resolved <- finishResolverWithin resolver
        resolved |> shouldBe (DnsResolved [])

    it "uses one shared deadline and waits for both lookup finalizers" \_ -> do
      started <- Channel.newBounded 2 :: Task Text (Channel.Channel DnsRecordType)
      finalizerEntered <- Channel.newBounded 2 :: Task Text (Channel.Channel DnsRecordType)
      finalized <- Channel.newBounded 2 :: Task Text (Channel.Channel DnsRecordType)
      deadlineCalls <- Channel.newBounded 2 :: Task Text (Channel.Channel Int)
      deadlineTrigger <- Channel.newBounded 1 :: Task Text (Channel.Channel Unit)
      finalizerRelease <- Channel.newBounded 2 :: Task Text (Channel.Channel Unit)
      neverRelease <- Channel.newBounded 1 :: Task Text (Channel.Channel Unit)
      let lookupRecord recordType _ =
            Task.finally
              (do
                  Channel.write recordType finalizerEntered
                  Channel.read finalizerRelease
                  Channel.write recordType finalized
              )
              (do
                  Channel.write recordType started
                  Channel.read neverRelease
                  Task.yield (DnsResolved [])
              )
      let waitForDeadline milliseconds = do
            Channel.write milliseconds deadlineCalls
            Channel.read deadlineTrigger
      let actions = DnsActions
            { lookupDnsRecord = lookupRecord,
              waitForDnsDeadline = waitForDeadline
            }
      withResolver (DnsConfig {timeoutSeconds = 7}) actions "example.org" \resolver -> do
        Task.finally (releaseFinalizers finalizerRelease) do
          firstFamily <- readWithin "first address family did not start" started
          secondFamily <- readWithin "second address family did not start" started
          assertBothFamilies firstFamily secondFamily
          observedDeadline <- readWithin "shared deadline did not start" deadlineCalls
          observedDeadline |> shouldBe 7000
          Channel.write unit deadlineTrigger
          finalizerStarted <- readWithin "lookup finalizer did not start" finalizerEntered
          case finalizerStarted of
            ARecord -> Task.yield unit
            AAAARecord -> Task.yield unit
          completionWhileFinalizersWait <- AsyncTask.race (AsyncTask.waitFor resolver) (AsyncTask.sleep 50)
          case completionWhileFinalizersWait of
            AsyncTask.LeftWon _ -> Task.throw "deadline returned before lookup finalizers were released"
            AsyncTask.RightWon _ -> Task.yield unit
          releaseFinalizers finalizerRelease
          resolved <- finishResolverWithin resolver
          resolved |> shouldBe DnsTimedOut
          firstFinalizer <- Channel.tryRead finalized
          secondFinalizer <- Channel.tryRead finalized
          case (firstFinalizer, secondFinalizer) of
            (Just first, Just second) -> assertBothFamilies first second
            _ -> Test.fail "deadline returned before both lookup finalizers finished"
          extraDeadline <- Channel.tryRead deadlineCalls
          extraDeadline |> shouldBe Nothing

    it "gives a family timeout precedence over the other family's success" \_ -> do
      deadlineGate <- Channel.newBounded 1 :: Task Text (Channel.Channel Unit)
      let lookupRecord recordType = case recordType of
            ARecord -> \_ -> Task.yield DnsTimedOut
            AAAARecord -> \_ -> Task.yield (DnsResolved ["2001:db8::1"])
      let actions = DnsActions
            { lookupDnsRecord = lookupRecord,
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      resolved <- runResolver (DnsConfig {timeoutSeconds = 3}) actions "example.org"
      resolved |> shouldBe DnsTimedOut

      let reverseLookupRecord recordType = case recordType of
            ARecord -> \_ -> Task.yield (DnsResolved ["192.0.2.10"])
            AAAARecord -> \_ -> Task.yield DnsTimedOut
      let reverseActions = DnsActions
            { lookupDnsRecord = reverseLookupRecord,
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      reverseResolved <- runResolver (DnsConfig {timeoutSeconds = 3}) reverseActions "example.org"
      reverseResolved |> shouldBe DnsTimedOut

    it "keeps A errors first and appends AAAA addresses after A addresses" \_ -> do
      deadlineGate <- Channel.newBounded 1 :: Task Text (Channel Unit)
      let failures = DnsActions
            { lookupDnsRecord = \recordType _ -> case recordType of
                ARecord -> Task.yield (DnsFailed (DnsSubprocessFailure "A lookup failed"))
                AAAARecord -> Task.yield (DnsFailed (DnsSubprocessFailure "AAAA lookup failed")),
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      failureResult <- runResolver (DnsConfig {timeoutSeconds = 3}) failures "example.org"
      failureResult |> shouldBe (DnsFailed (DnsSubprocessFailure "A lookup failed"))

      let successes = DnsActions
            { lookupDnsRecord = \recordType _ -> case recordType of
                ARecord -> Task.yield (DnsResolved ["192.0.2.10", "192.0.2.11"])
                AAAARecord -> Task.yield (DnsResolved ["2001:db8::10"]),
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      orderedResult <- runResolver (DnsConfig {timeoutSeconds = 3}) successes "example.org"
      orderedResult |> shouldBe (DnsResolved ["192.0.2.10", "192.0.2.11", "2001:db8::10"])

    it "accepts either family's addresses when the other family fails" \_ -> do
      deadlineGate <- Channel.newBounded 1 :: Task Text (Channel.Channel Unit)
      let aOnly = DnsActions
            { lookupDnsRecord = \recordType _ -> case recordType of
                ARecord -> Task.yield (DnsResolved ["192.0.2.10"])
                AAAARecord -> Task.yield (DnsFailed (DnsSubprocessFailure "No AAAA records")),
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      aOnlyResult <- runResolver (DnsConfig {timeoutSeconds = 3}) aOnly "example.org"
      aOnlyResult |> shouldBe (DnsResolved ["192.0.2.10"])

      let aaaaOnly = DnsActions
            { lookupDnsRecord = \recordType _ -> case recordType of
                ARecord -> Task.yield (DnsFailed (DnsSubprocessFailure "No A records"))
                AAAARecord -> Task.yield (DnsResolved ["2001:db8::10"]),
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      aaaaOnlyResult <- runResolver (DnsConfig {timeoutSeconds = 3}) aaaaOnly "example.org"
      aaaaOnlyResult |> shouldBe (DnsResolved ["2001:db8::10"])

    it "turns a typed Task failure into the private task failure result" \_ -> do
      deadlineGate <- Channel.newBounded 1 :: Task Text (Channel.Channel Unit)
      let actions = DnsActions
            { lookupDnsRecord = \recordType _ -> case recordType of
                ARecord -> Task.throw (DnsSubprocessFailure "hidden task failure")
                AAAARecord -> Task.yield (DnsFailed (DnsSubprocessFailure "AAAA failed")),
              waitForDnsDeadline = \_ -> Channel.read deadlineGate
            }
      resolved <- runResolver (DnsConfig {timeoutSeconds = 3}) actions "example.org"
      resolved |> shouldBe (DnsFailed DnsTaskFailure)
      errorMessage DnsTaskFailure |> shouldBe "Internal error"


startResolver ::
  DnsConfig ->
  DnsActions ->
  Text ->
  Task Text (AsyncTask.AsyncTask Text (Result DnsError DnsResult))
startResolver config actions hostname =
  resolveWith config actions hostname
    |> Task.asResult
    |> AsyncTask.run


withResolver :: DnsConfig -> DnsActions -> Text -> (AsyncTask.AsyncTask Text (Result DnsError DnsResult) -> Task Text result) -> Task Text result
withResolver config actions hostname useResolver = do
  resolver <- startResolver config actions hostname
  Task.finally (AsyncTask.cancel resolver) (useResolver resolver)


readWithin :: Text -> Channel.Channel value -> Task Text value
readWithin message channel = do
  result <- AsyncTask.race (Channel.read channel) (AsyncTask.sleep 1000)
  case result of
    AsyncTask.LeftWon value -> Task.yield value
    AsyncTask.RightWon _ -> Task.throw message


releaseFinalizers :: Channel.Channel Unit -> Task Text Unit
releaseFinalizers channel = do
  Channel.write unit channel
  Channel.write unit channel


finishResolverWithin :: AsyncTask.AsyncTask Text (Result DnsError DnsResult) -> Task Text DnsResult
finishResolverWithin resolver = do
  completion <- AsyncTask.race (AsyncTask.waitFor resolver) (AsyncTask.sleep 3000)
  case completion of
    AsyncTask.LeftWon (Ok result) -> Task.yield result
    AsyncTask.LeftWon (Err _) -> Task.throw "resolver Task failed"
    AsyncTask.RightWon _ -> do
      AsyncTask.cancel resolver
      Task.throw "resolver did not finish after the controlled outcome"


runResolver :: DnsConfig -> DnsActions -> Text -> Task Text DnsResult
runResolver config actions hostname = do
  resolver <- startResolver config actions hostname
  finishResolverWithin resolver


assertBothFamilies :: DnsRecordType -> DnsRecordType -> Task Text Unit
assertBothFamilies first second = case (first, second) of
  (ARecord, AAAARecord) -> Task.yield unit
  (AAAARecord, ARecord) -> Task.yield unit
  _ -> Test.fail "both address families did not start"
