module Record (Record, Fields, MergeFields, Extends, overrideWith, merge) where

import Data.Record.Anon qualified
import Data.Record.Anon.Simple (Record)
import Data.Record.Anon.Simple qualified


type Fields fields = Data.Record.Anon.Row fields


type Extends fields1 fields2 = Data.Record.Anon.SubRow fields1 fields2


type MergeFields (fields1 :: Fields a) (fields2 :: Fields a) = Data.Record.Anon.Merge fields1 fields2


overrideWith :: (Extends self overrides) => Record self -> Record overrides -> Record self
overrideWith overrides self = Data.Record.Anon.Simple.inject self overrides


merge :: Record fields1 -> Record fields2 -> Record (MergeFields fields1 fields2)
merge record1 record2 = Data.Record.Anon.Simple.merge record1 record2
