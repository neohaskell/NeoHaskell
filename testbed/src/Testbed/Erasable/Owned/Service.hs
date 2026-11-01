{-| Register the application-owned protected record and erasure commands.
The outbound handler records the result through 'RecordErasureResult'.
-}
module Testbed.Erasable.Owned.Service (service) where

import Core
import Service qualified
import Testbed.Erasable.Owned.Commands.Create (CreateOwnedProtected)
import Testbed.Erasable.Owned.Commands.MapErased (MapErasedSecret)
import Testbed.Erasable.Owned.Commands.Record (RecordErasureResult)
import Testbed.Erasable.Owned.Commands.Request (RequestErasure)
import Testbed.Erasable.Owned.Core ()


service :: Service _ _
service = Service.new
  |> Service.command @CreateOwnedProtected
  |> Service.command @MapErasedSecret
  |> Service.command @RequestErasure
  |> Service.command @RecordErasureResult
