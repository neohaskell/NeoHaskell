module Service.Transport.Internal (
  InternalTransport (..),
) where

import Basics
import Service.Command.Core (NameOf)


data InternalTransport = InternalTransport
  deriving (Eq, Show)


type instance NameOf InternalTransport = "InternalTransport"
