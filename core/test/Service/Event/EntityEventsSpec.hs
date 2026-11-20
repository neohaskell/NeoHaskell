-- | Verify event sums generated from explicit payload lists.
--
-- The fixtures exercise generated constructors, Decider wrapping, routing, and typed storage.
module Service.Event.EntityEventsSpec where

import Array qualified
import Core
import Decider qualified
import Json qualified
import Language.Haskell.TH.Syntax qualified as TH
import Service.Entity.Core qualified as Entity
import Service.Event qualified as Event
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Stream qualified
import Task qualified
import Test
import Uuid qualified


data Proposal = Proposal {created :: Bool, uploadCount :: Int}


data ProposalCreated = ProposalCreated {proposalId :: Uuid}
  deriving (Eq)


deriveEvent ''ProposalCreated


type instance EntityOf ProposalCreated = Proposal


data PdfUploaded = PdfUploaded {proposalId :: Uuid, pdfRef :: Text}
  deriving (Eq)


deriveEvent ''PdfUploaded


type instance EntityOf PdfUploaded = Proposal


deriveEntityEvents ''Proposal [''ProposalCreated, ''PdfUploaded]


instance Entity Proposal where
  initialStateImpl = Proposal {created = False, uploadCount = 0}
  updateImpl eventValue proposal = case eventValue of
    ProposalEventProposalCreated _ -> proposal {created = True}
    ProposalEventPdfUploaded _ -> proposal {uploadCount = proposal.uploadCount + 1}


instance Entity.Event ProposalCreated where
  getEventEntityIdImpl payload = payload.proposalId


instance Entity.Event PdfUploaded where
  getEventEntityIdImpl payload = payload.proposalId


decideCreate :: ProposalCreated -> Decision ProposalEvent
decideCreate payload = Decider.acceptNew [payload]


decideUpload :: PdfUploaded -> Decision ProposalEvent
decideUpload payload = Decider.acceptExisting [payload]


data PingEntity = PingEntity


instance Entity PingEntity where
  initialStateImpl = PingEntity
  updateImpl _ entity = entity


data Pinged = Pinged {pingId :: Uuid}
  deriving (Eq)


deriveEvent ''Pinged


type PingOwner = PingEntity


type instance EntityOf Pinged = PingOwner


instance Entity.Event Pinged where
  getEventEntityIdImpl payload = payload.pingId


deriveEntityEvents ''PingEntity [''Pinged]


data EmptyEntity = EmptyEntity


$(TH.recover
    [d| rejectsEmptyPayloadList :: Bool; rejectsEmptyPayloadList = True |]
    do
      _ <- deriveEntityEvents ''EmptyEntity []
      [d| rejectsEmptyPayloadList :: Bool; rejectsEmptyPayloadList = False |])


data DuplicateEntity = DuplicateEntity


data DuplicatePayload = DuplicatePayload


type instance EntityOf DuplicatePayload = DuplicateEntity


$(TH.recover
    [d| rejectsDuplicatePayload :: Bool; rejectsDuplicatePayload = True |]
    do
      _ <- deriveEntityEvents ''DuplicateEntity [''DuplicatePayload, ''DuplicatePayload]
      [d| rejectsDuplicatePayload :: Bool; rejectsDuplicatePayload = False |])


data OtherEntity = OtherEntity


instance Entity OtherEntity where
  initialStateImpl = OtherEntity
  updateImpl _ entity = entity


$(TH.recover
    [d| rejectsForeignPayload :: Bool; rejectsForeignPayload = True |]
    do
      _ <- deriveEntityEvents ''OtherEntity [''ProposalCreated]
      [d| rejectsForeignPayload :: Bool; rejectsForeignPayload = False |])


data UnownedEntity = UnownedEntity


data UnownedPayload = UnownedPayload


$(TH.recover
    [d| rejectsMissingOwnership :: Bool; rejectsMissingOwnership = True |]
    do
      _ <- deriveEntityEvents ''UnownedEntity [''UnownedPayload]
      [d| rejectsMissingOwnership :: Bool; rejectsMissingOwnership = False |])


data CollisionEntity = CollisionEntity


data CollisionPayload = CollisionPayload


type instance EntityOf CollisionPayload = CollisionEntity


$(TH.recover
    [d| rejectsConstructorCollision :: Bool; rejectsConstructorCollision = True |]
    do
      _ <- deriveEntityEvents ''CollisionEntity [TH.mkName "CollisionPayload", ''CollisionPayload]
      [d| rejectsConstructorCollision :: Bool; rejectsConstructorCollision = False |])


data Unlisted = Unlisted


$(do
    mappings <- TH.reifyInstances ''EventVariantOf [TH.ConT ''ProposalEvent, TH.ConT ''Unlisted]
    case mappings of
      [] -> [d| hasNoUnlistedMapping :: Bool; hasNoUnlistedMapping = True |]
      _ -> [d| hasNoUnlistedMapping :: Bool; hasNoUnlistedMapping = False |])


spec :: Spec Unit
spec = do
  describe "deriveEntityEvents" do
    it "generates both payload mappings and skips unrelated constructors" \_ -> do
      let created = ProposalCreated {proposalId = Uuid.nil}
      let uploaded = PdfUploaded {proposalId = Uuid.nil, pdfRef = "pdf"}
      let createdEvent = fromVariant created :: ProposalEvent
      let uploadedEvent = fromVariant uploaded :: ProposalEvent
      case createdEvent of
        ProposalEventProposalCreated payload -> payload |> shouldBe created
        ProposalEventPdfUploaded _ -> Test.fail "wrong constructor for ProposalCreated"
      case uploadedEvent of
        ProposalEventPdfUploaded payload -> payload |> shouldBe uploaded
        ProposalEventProposalCreated _ -> Test.fail "wrong constructor for PdfUploaded"
      (toVariant createdEvent :: Maybe ProposalCreated) |> shouldBe (Just created)
      (toVariant uploadedEvent :: Maybe PdfUploaded) |> shouldBe (Just uploaded)
      (toVariant createdEvent :: Maybe PdfUploaded) |> shouldBe Nothing
      (toVariant uploadedEvent :: Maybe ProposalCreated) |> shouldBe Nothing

    it "wraps plain payloads in acceptNew and acceptExisting decisions" \_ -> do
      let created = ProposalCreated {proposalId = Uuid.nil}
      let uploaded = PdfUploaded {proposalId = Uuid.nil, pdfRef = "pdf"}
      let newDecision = decideCreate created
      let existingDecision = decideUpload uploaded
      case newDecision of
        Decider.Accept StreamCreation events ->
          case Array.first events of
            Just (ProposalEventProposalCreated payload) -> payload |> shouldBe created
            _ -> Test.fail "acceptNew did not wrap ProposalCreated"
        _ -> Test.fail "acceptNew did not request stream creation"
      case existingDecision of
        Decider.Accept ExistingStream events ->
          case Array.first events of
            Just (ProposalEventPdfUploaded payload) -> payload |> shouldBe uploaded
            _ -> Test.fail "acceptExisting did not wrap PdfUploaded"
        _ -> Test.fail "acceptExisting did not require an existing stream"

    it "keeps mixed payload order through the event helper" \_ -> do
      let created = ProposalCreated {proposalId = Uuid.nil}
      let uploaded = PdfUploaded {proposalId = Uuid.nil, pdfRef = "pdf"}
      let decision =
            Decider.acceptExisting
              [ event created :: ProposalEvent
              , event uploaded :: ProposalEvent
              ]
      case decision of
        Decider.Accept ExistingStream events -> do
          case Array.get 0 events of
            Just (ProposalEventProposalCreated payload) -> payload |> shouldBe created
            _ -> Test.fail "first mixed payload changed"
          case Array.get 1 events of
            Just (ProposalEventPdfUploaded payload) -> payload |> shouldBe uploaded
            _ -> Test.fail "second mixed payload changed"
        _ -> Test.fail "mixed decision was not accepted"

    it "routes each generated constructor through its payload" \_ -> do
      let proposalId = Uuid.generateV5 Uuid.nil "proposal"
      let created = ProposalEventProposalCreated (ProposalCreated {proposalId})
      let uploaded = ProposalEventPdfUploaded (PdfUploaded {proposalId, pdfRef = "pdf"})
      Entity.getEventEntityIdImpl created |> shouldBe proposalId
      Entity.getEventEntityIdImpl uploaded |> shouldBe proposalId

    it "updates state through constructors generated before the reducer" \_ -> do
      let created = ProposalEventProposalCreated (ProposalCreated {proposalId = Uuid.nil})
      let uploaded = ProposalEventPdfUploaded (PdfUploaded {proposalId = Uuid.nil, pdfRef = "pdf"})
      let initial = initialStateImpl @Proposal
      let updated = updateImpl uploaded (updateImpl created initial)
      updated.created |> shouldBe True
      updated.uploadCount |> shouldBe 1

    it "generates a single-payload sum with ownership and JSON" \_ -> do
      let original = PingEventPinged (Pinged {pingId = Uuid.nil})
      let owned = original :: EventOf PingEntity
      let extracted = toVariant owned :: Maybe Pinged
      let expected =
            Json.object
              [ "tag" Json..= ("PingEventPinged" :: Text)
              , "contents" Json..= Pinged {pingId = Uuid.nil}
              ]
      extracted |> shouldBe (Just (Pinged {pingId = Uuid.nil}))
      Json.encode original |> shouldBe expected
      case Json.decode @PingEvent (Json.encode original) of
        Ok (PingEventPinged payload) -> payload |> shouldBe Pinged {pingId = Uuid.nil}
        Err reason -> Test.fail reason

    it "keeps tagged JSON when the parent has multiple payloads" \_ -> do
      let payload = ProposalCreated {proposalId = Uuid.nil}
      let original = ProposalEventProposalCreated payload
      let expected =
            Json.object
              [ "tag" Json..= ("ProposalEventProposalCreated" :: Text)
              , "contents" Json..= payload
              ]
      Json.encode original |> shouldBe expected
      case Json.decode @ProposalEvent expected of
        Ok (ProposalEventProposalCreated decoded) -> decoded |> shouldBe payload
        Ok _ -> Test.fail "tagged JSON selected another constructor"
        Err reason -> Test.fail reason

    it "serializes generated parent events through the typed EventStore" \_ -> do
      rawStore <- InMemory.new |> Task.mapError toText
      let store = EventStore.castEventStore @ProposalEvent rawStore
      let entityName = Event.EntityName "Proposal"
      let streamId = Event.StreamId "proposal-generator-fixture"
      let uploaded = PdfUploaded {proposalId = Uuid.nil, pdfRef = "pdf"}
      case decideUpload uploaded of
        Decider.Accept ExistingStream events -> do
          payload <- Event.payloadFromEvents entityName streamId events
          _ <- store.insert payload |> Task.mapError toText
          messages <-
            store.readAllStreamEvents entityName streamId
              |> Task.mapError toText
              |> Task.andThen Stream.toArray
          let stored = EventStore.collectStreamEvents messages |> Array.map (\value -> value.event)
          case Array.first stored of
            Just (ProposalEventPdfUploaded decoded) -> decoded |> shouldBe uploaded
            _ -> Test.fail "typed EventStore did not decode the generated parent event"
        _ -> Test.fail "plain payload did not produce an existing-stream decision"
      store.close

    it "rejects missing, duplicate, and foreign payload declarations" \_ -> do
      rejectsEmptyPayloadList |> shouldBe True
      rejectsDuplicatePayload |> shouldBe True
      rejectsForeignPayload |> shouldBe True
      rejectsMissingOwnership |> shouldBe True
      rejectsConstructorCollision |> shouldBe True
      hasNoUnlistedMapping |> shouldBe True
