module Auth.UrlValidation.Dns (
  DnsConfig (..),
  DnsActions (..),
  DnsRecordType (..),
  DnsError (..),
  DnsResult (..),
  resolve,
  resolveWith,
  errorMessage,
) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Auth.UrlValidation.Dns.Error (DnsError (..), errorMessage)
import Auth.UrlValidation.Dns.RecordType (DnsRecordType (..))
import Auth.UrlValidation.Dns.RecordType qualified as RecordType
import Auth.UrlValidation.Dns.Result (DnsResult (..), combine)
import Basics
import Environment qualified
import LinkedList qualified
import Maybe (Maybe (..))
import Result (Result (..))
import System.Exit qualified as GhcExit
import System.Process qualified as GhcProcess
import System.Timeout qualified as GhcTimeout
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Text.Read qualified as GhcRead


data DnsConfig = DnsConfig
  { timeoutSeconds :: Int
  }
  deriving (Eq, Show)


data DnsActions = DnsActions
  { lookupDnsRecord :: DnsRecordType -> Text -> Task DnsError DnsResult,
    waitForDnsDeadline :: Int -> Task DnsError Unit
  }


data DnsSubprocessResult
  = DnsSubprocessSuccess Text
  | DnsSubprocessTimeout
  | DnsSubprocessError Text
  deriving (Eq, Show)


defaultDnsConfig :: DnsConfig
defaultDnsConfig = DnsConfig {timeoutSeconds = 5}


getDnsConfig :: Task DnsError DnsConfig
getDnsConfig = do
  envResult <- Environment.getVariable "DNS_TIMEOUT_SECONDS" |> Task.asResult
  case envResult of
    Err _ -> Task.yield defaultDnsConfig
    Ok envValue -> do
      case GhcRead.readMaybe @Int (Text.toLinkedList envValue) of
        Nothing -> Task.yield defaultDnsConfig
        Just seconds ->
          case seconds >= 1 && seconds <= 60 of
            True -> Task.yield (DnsConfig {timeoutSeconds = seconds})
            False -> Task.yield defaultDnsConfig


resolve :: Text -> Task DnsError DnsResult
resolve hostname = do
  config <- getDnsConfig
  let actions = DnsActions
        { lookupDnsRecord = resolveDnsRecordType config.timeoutSeconds,
          waitForDnsDeadline = AsyncTask.sleep
        }
  resolveWith config actions hostname


resolveWith :: DnsConfig -> DnsActions -> Text -> Task DnsError DnsResult
resolveWith config actions hostname = do
  let captureLookupFailure recordType = do
        lookupResult <- actions.lookupDnsRecord recordType hostname |> Task.asResult
        case lookupResult of
          Ok dnsResult -> Task.yield dnsResult
          Err _ -> Task.yield (DnsFailed DnsTaskFailure)
  let familyLookups = AsyncTask.runConcurrently
        (captureLookupFailure ARecord, captureLookupFailure AAAARecord)
  let deadlineMilliseconds = config.timeoutSeconds * 1000
  let deadline = actions.waitForDnsDeadline deadlineMilliseconds |> Task.map (\_ -> DnsTimedOut)
  raceResult <- AsyncTask.race familyLookups deadline
  case raceResult of
    AsyncTask.LeftWon (ipv4Result, ipv6Result) ->
      Task.yield (combine ipv4Result ipv6Result)
    AsyncTask.RightWon timeoutResult -> Task.yield timeoutResult


resolveDnsRecordType :: Int -> DnsRecordType -> Text -> Task DnsError DnsResult
resolveDnsRecordType timeoutSeconds recordType hostname = do
  let timeoutArg = Text.fromLinkedList [fmt|-W#{timeoutSeconds}|]
  let hostArgs = Array.fromLinkedList [timeoutArg, "-t", RecordType.hostArgument recordType, hostname]
  completion <- runDnsSubprocess timeoutSeconds "host" hostArgs
  case completion of
    DnsSubprocessTimeout -> Task.yield DnsTimedOut
    DnsSubprocessError errMsg -> Task.yield (DnsFailed (DnsSubprocessFailure errMsg))
    DnsSubprocessSuccess output -> do
      let ips = parseHostOutput recordType output
      case ips of
        [] -> Task.yield (DnsFailed DnsNoAddressesResolved)
        _ -> Task.yield (DnsResolved ips)


runDnsSubprocess ::
  forall error.
  Int ->
  Text ->
  Array Text ->
  Task error DnsSubprocessResult
runDnsSubprocess timeoutSeconds cmd args = do
  let cmdStr = Text.toLinkedList cmd
  let argsStr = Array.map Text.toLinkedList args |> Array.toLinkedList
  let timeoutMicros = timeoutSeconds * 1000000
  let process = GhcProcess.proc cmdStr argsStr
  result <-
    GhcTimeout.timeout timeoutMicros (GhcProcess.readCreateProcessWithExitCode process "")
      |> Task.fromIO
      |> Task.asResultSafe @Text
  case result of
    Err errMsg -> Task.yield (DnsSubprocessError errMsg)
    Ok Nothing -> Task.yield DnsSubprocessTimeout
    Ok (Just (exitCode, stdoutStr, stderrStr)) -> do
      let out = Text.fromLinkedList stdoutStr
      let err = Text.fromLinkedList stderrStr
      case exitCode of
        GhcExit.ExitSuccess -> Task.yield (DnsSubprocessSuccess out)
        GhcExit.ExitFailure 1 ->
          Task.yield (DnsSubprocessError (Text.fromLinkedList [fmt|Host not found: #{out}|]))
        GhcExit.ExitFailure code -> do
          let errMsg = case Text.isEmpty err of
                True -> Text.fromLinkedList [fmt|DNS resolution failed (exit code #{code})|]
                False -> err
          Task.yield (DnsSubprocessError errMsg)


parseHostOutput :: DnsRecordType -> Text -> [Text]
parseHostOutput recordType output = do
  let rawLines = Text.lines output |> Array.toLinkedList
  let extractIp line = do
        let parts = Text.words line |> Array.toLinkedList
        case recordType of
          ARecord -> extractIPv4FromHostLine parts
          AAAARecord -> extractIPv6FromHostLine parts
  let allIps = LinkedList.filterMap extractIp rawLines
  removeDuplicates allIps


extractIPv4FromHostLine :: [Text] -> Maybe Text
extractIPv4FromHostLine parts = case parts of
  (_ : "has" : "address" : ip : _) -> Just ip
  _ -> Nothing


extractIPv6FromHostLine :: [Text] -> Maybe Text
extractIPv6FromHostLine parts = case parts of
  (_ : "has" : "IPv6" : "address" : ip : _) -> Just ip
  _ -> Nothing


removeDuplicates :: forall element. (Eq element) => [element] -> [element]
removeDuplicates items = case items of
  [] -> []
  (x : xs) -> x : removeDuplicates (LinkedList.filter (\y -> y != x) xs)
