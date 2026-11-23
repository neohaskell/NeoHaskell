-- | Record meter readings and project the latest reading for each meter.
-- Register 'meterFlow' to exercise the same typed declarations as the generated starter.
module Testbed.Meter (meterFlow) where

import Core
import Decider qualified
import Derive
import Service.AccessControl (AccessError, UserClaims, publicAccess, publicView)
import Service.Auth (RequestContext)
import Service.EventModel qualified as EventModel
import Uuid qualified


data ReadingRecorded = ReadingRecorded
  { entityId :: Uuid,
    reading :: Int
  }


derive ''ReadingRecorded eventPayload


data MeterEntity = MeterEntity
  { meterId :: Uuid,
    latest :: Int
  }


derive ''MeterEntity entity { events = [''ReadingRecorded] }


initialState :: MeterEntity
initialState = MeterEntity {meterId = Uuid.nil, latest = 0}


update :: MeterEvent -> MeterEntity -> MeterEntity
update change _ = case change of
  MeterEventReadingRecorded payload ->
    MeterEntity {meterId = payload.entityId, latest = payload.reading}


data RecordReading = RecordReading
  { firstReading :: Int
  }


getEntityId :: RecordReading -> Maybe Uuid
getEntityId _ = Nothing


decide :: RecordReading -> Maybe MeterEntity -> RequestContext -> Decision MeterEvent
decide request _ _ = do
  meterId <- Decider.generateUuid
  Decider.acceptNew [ReadingRecorded {entityId = meterId, reading = request.firstReading}]


derive ''RecordReading command { entity = ''MeterEntity }


data MeterReading = MeterReading
  { reading :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = publicAccess


canView :: Maybe UserClaims -> MeterReading -> Maybe AccessError
canView = publicView


projectionIdFromMeterEntity :: MeterEntity -> Uuid
projectionIdFromMeterEntity meter = meter.meterId


combineMeterEntity :: MeterEntity -> Maybe MeterReading -> QueryAction MeterReading
combineMeterEntity meter _ = Update MeterReading {reading = meter.latest}


derive ''MeterReading query { entities = [''MeterEntity] }


meterFlow :: EventModel.FeatureAction
meterFlow = EventModel.feature "Meter readings" do
  EventModel.chapter "Meter" do
    EventModel.slice "Record a reading" do
      EventModel.entity @MeterEntity do
        EventModel.command @RecordReading do
          EventModel.produces @ReadingRecorded
    EventModel.slice "Show the latest reading" do
      EventModel.query @MeterReading do
        EventModel.readsFrom @MeterEntity do
          EventModel.onEvent @ReadingRecorded
