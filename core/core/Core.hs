-- | # Core
--
-- This module is automatically imported in all the NeoHaskell files,
module Core (
  module Reexported,
) where

import Appendable as Reexported ((++))
import Array as Reexported (Array)
import Basics as Reexported
import Bytes as Reexported (Bytes)
import Channel as Reexported (Channel)
import Char as Reexported (Char)
import ConcurrentVar as Reexported (ConcurrentVar)
import Console as Reexported (log, print, readLine)
import DateTime as Reexported (DateTime)
import Decider as Reexported (CommandResult (..), Decision)
import Default as Reexported (Default (..), defaultValue)
import Documented as Reexported (Documented (..))
import DurableChannel as Reexported (DurableChannel)
import Function as Reexported
import IO as Reexported (IO)
import LinkedList as Reexported (LinkedList)
import Lock as Reexported (Lock)
import Map as Reexported (Map)
import Maybe as Reexported (Maybe (..))
import Path as Reexported (Path, path)
import Record as Reexported (KnownHash (..), Record)
import Result as Reexported (Result (..))
import Schema as Reexported (Schema (..), FieldSchema (..), ToSchema (..))
import Service.Command as Reexported (Command (..), NameOf)
import Service.Entity as Reexported (Entity (..), EntityOf, EventOf)
import Service.Event as Reexported (InsertionType (..), StreamId, ToStreamId (..))
import Service.Query as Reexported (EntitiesOf, Query (..), QueryAction (..), QueryOf (..))
import Service.ServiceDefinition.Core as Reexported (
  Service,
 )
import Task as Reexported (Task)
import Text as Reexported (Text)
import ToText as Reexported (ToText, toPrettyText, toText)
import Trigger as Reexported (Trigger)
import Type.Reflection as Reexported (Typeable)
import Unknown as Reexported (Unknown)
import Uuid as Reexported (Uuid)
import Var as Reexported (Var)
import Version as Reexported (Version, version)
