module Record (
  module Reexported,
  Record,
  ContextRecord,
) where

import Data.Record.Anon as Reexported
import Data.Record.Anon.Advanced as Reexported hiding (Record)
import Data.Record.Anon.Advanced qualified as AdvRecord


type Record r = AdvRecord.Record I r


type ContextRecord = AdvRecord.Record