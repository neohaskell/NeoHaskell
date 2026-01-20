module Service.MockTransport (
  MockTransport (..),
  MockTransport2 (..),
  server,
  server2,
) where

import Core
import Json qualified
import Record qualified
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Service.Transport (EndpointHandler, Endpoints (..), Transport (..))
import Task qualified


-- | A mock transport for testing purposes.
-- Does not actually run any server.
data MockTransport = MockTransport
  { name :: Text
  }


type instance NameOf MockTransport = "MockTransport"


deriveKnownHash "MockTransport"


-- | Default MockTransport.
server :: MockTransport
server = MockTransport {name = "mock1"}


-- | A second mock transport type for testing multiple transports.
data MockTransport2 = MockTransport2
  { name :: Text
  }


type instance NameOf MockTransport2 = "MockTransport2"


deriveKnownHash "MockTransport2"


-- | Second MockTransport for testing multiple transports.
server2 :: MockTransport2
server2 = MockTransport2 {name = "mock2"}


instance Transport MockTransport where
  type Request MockTransport = Bytes
  type Response MockTransport = Bytes
  type RunnableTransport MockTransport = Unit


  assembleTransport :: Endpoints MockTransport -> Unit
  assembleTransport _ = unit


  runTransport :: MockTransport -> Unit -> Task Text Unit
  runTransport _ _ = Task.yield unit


  buildHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    MockTransport ->
    Record.Proxy command ->
    (RequestContext -> command -> Task Text CommandResponse) ->
    EndpointHandler
  buildHandler _ _ _ _ctx _req respond = do
    let mockResponse = Response.Accepted {entityId = "mock-id"}
    respond (mockResponse, "")


instance Transport MockTransport2 where
  type Request MockTransport2 = Bytes
  type Response MockTransport2 = Bytes
  type RunnableTransport MockTransport2 = Unit


  assembleTransport :: Endpoints MockTransport2 -> Unit
  assembleTransport _ = unit


  runTransport :: MockTransport2 -> Unit -> Task Text Unit
  runTransport _ _ = Task.yield unit


  buildHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    MockTransport2 ->
    Record.Proxy command ->
    (RequestContext -> command -> Task Text CommandResponse) ->
    EndpointHandler
  buildHandler _ _ _ _ctx _req respond = do
    let mockResponse = Response.Accepted {entityId = "mock-id"}
    respond (mockResponse, "")
