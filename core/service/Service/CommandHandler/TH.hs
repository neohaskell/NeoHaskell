module Service.CommandHandler.TH (
  deriveCommand,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
import Data.List qualified as GhcList
import GHC.Base (String)
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Ppr qualified as THPpr
import Language.Haskell.TH.Syntax qualified as TH


data MultiTenancyMode
  = MustHaveUuid
  | MustNotHaveUuid
  deriving (Eq, Show)


data FunctionInfo = FunctionInfo
  { functionName :: String,
    thName :: TH.Name,
    expectedSignatureWithUuid :: String,
    expectedSignatureWithoutUuid :: String
  }
  deriving (Generic)


determineMultiTenancyMode :: Maybe TH.Name -> TH.Q MultiTenancyMode
determineMultiTenancyMode maybeMultiTenancy = do
  case maybeMultiTenancy of
    Just multiTenancyName -> do
      multiTenancyInfo <- TH.reify multiTenancyName
      let isMultiTenancyTrue = case multiTenancyInfo of
            TH.TyConI (TH.TySynD _ _ (TH.PromotedT trueName)) ->
              TH.nameBase trueName == "True"
            _ -> False
      if isMultiTenancyTrue
        then pure MustHaveUuid
        else pure MustNotHaveUuid
    Nothing -> pure MustNotHaveUuid


checkTypeContainsUuid :: String -> Bool
checkTypeContainsUuid typeStr = do
  let words = typeStr |> GhcList.words
  "Uuid.Uuid" `GhcList.elem` words


buildErrorMessage ::
  String ->
  String ->
  String ->
  MultiTenancyMode ->
  String
buildErrorMessage functionName expectedSig currentSig mode = do
  let modeDescription :: String
      modeDescription = case mode of
        MustHaveUuid ->
          "When MultiTenancy is set to True"
        MustNotHaveUuid ->
          "When MultiTenancy is False or undefined"
  let additionalHelp :: String
      additionalHelp = case mode of
        MustHaveUuid ->
          [fmt|Please update your #{functionName} function to accept a Uuid tenant ID as the first argument.|]
        MustNotHaveUuid ->
          [fmt|The function should NOT have Uuid as a parameter when MultiTenancy is False.
If you need multi-tenancy, define: type MultiTenancy = True|]
  [fmt|
ERROR: '#{functionName}' has incorrect signature.

#{modeDescription}:
Expected signature:
  #{expectedSig}

Current signature:
  #{currentSig}

#{additionalHelp}
|]


validateFunctionSignature ::
  FunctionInfo ->
  MultiTenancyMode ->
  TH.Name ->
  TH.Q ()
validateFunctionSignature funcInfo mode _commandName = do
  functionInfo <- TH.reify funcInfo.thName
  case functionInfo of
    TH.VarI _ functionType _ -> do
      let typeStr = THPpr.pprint functionType
      let hasUuid = checkTypeContainsUuid typeStr
      let isValid = case mode of
            MustHaveUuid -> hasUuid
            MustNotHaveUuid -> not hasUuid
      if isValid
        then pure ()
        else do
          let expectedSig = case mode of
                MustHaveUuid -> funcInfo.expectedSignatureWithUuid
                MustNotHaveUuid -> funcInfo.expectedSignatureWithoutUuid
          let errorMsg = buildErrorMessage funcInfo.functionName expectedSig typeStr mode
          MonadFail.fail errorMsg
    _ -> pure ()


deriveCommand :: TH.Name -> THLib.DecsQ
deriveCommand someName = do
  let orError errMsg mVal =
        case mVal of
          Just x -> pure x
          Nothing -> MonadFail.fail errMsg

  let commandNameStr = TH.nameBase someName

  maybeEntityType <- TH.lookupTypeName "EntityOf"
  maybeGetEntityId <- TH.lookupValueName "getEntityId"
  maybeDecide <- TH.lookupValueName "decide"

  entityType <-
    maybeEntityType
      |> orError
        [fmt|
ERROR: Missing EntityOf type instance for command '#{commandNameStr}'.

Commands need to specify which entity type they operate on. This allows the command
handler to fetch the current state of the entity before deciding whether to accept
or reject the command.

Please add the following type instance to your module:

  type instance EntityOf #{commandNameStr} = YourEntityType

Example:
  -- If your command operates on a Cart entity:
  type instance EntityOf #{commandNameStr} = CartEntity

For reference, refer to the documentation: FIXME: ADD DOCS LINK
|]

  getEntityId <-
    maybeGetEntityId
      |> orError
        [fmt|
ERROR: Missing 'getEntityId' function for command '#{commandNameStr}'.

The 'getEntityId' function extracts the entity identifier from your command.
This ID is used to fetch the current state of the entity from the event store.

Please add the following function to your module:

  getEntityId :: #{commandNameStr} -> Maybe Text
  getEntityId command = ...

If your command creates a new entity (no existing ID), return Nothing:
  getEntityId _ = Nothing

If your command operates on an existing entity, extract and return its ID:
  getEntityId cmd = Just cmd.entityId

For reference, see: core/test/Integration/App/Cart/Commands/CreateCart.hs
|]

  decide <-
    maybeDecide
      |> orError
        [fmt|
ERROR: Missing 'decide' function for command '#{commandNameStr}'.

The 'decide' function contains your business logic. It receives the command and
the current entity state (if it exists), and returns a Decision to either accept
or reject the command.

Please add the following function to your module:

  decide :: #{commandNameStr} -> Maybe YourEntityType -> Decision YourEventType
  decide command maybeEntity = ...

Example implementations:

  -- For creating a new entity:
  decide _ entity = do
    case entity of
      Just _ -> Decision.reject "Entity already exists"
      Nothing -> Decision.acceptNew [EntityCreated]

  -- For updating an existing entity:
  decide cmd entity = do
    case entity of
      Nothing -> Decision.reject "Entity not found"
      Just existingEntity -> Decision.accept existingEntity [EntityUpdated]

For reference, see: core/test/Integration/App/Cart/Commands/CreateCart.hs
|]

  maybeMultiTenancy <- TH.lookupTypeName "MultiTenancy"
  commandClassName <-
    TH.lookupTypeName "Command"
      >>= orError
        [fmt|
ERROR: Command type class not found.

This is an internal error - the Command type class should be available from
Service.Command.Core. Please ensure you have imported the necessary modules.
|]

  multiTenancyMode <- determineMultiTenancyMode maybeMultiTenancy

  let entityTypeStr = TH.nameBase entityType

  let getEntityIdFuncInfo =
        FunctionInfo
          { functionName = "getEntityId",
            thName = getEntityId,
            expectedSignatureWithUuid = "getEntityId :: Uuid -> " ++ commandNameStr ++ " -> Maybe Text",
            expectedSignatureWithoutUuid = "getEntityId :: " ++ commandNameStr ++ " -> Maybe Text"
          }

  let decideFuncInfo =
        FunctionInfo
          { functionName = "decide",
            thName = decide,
            expectedSignatureWithUuid =
              "decide :: Uuid -> " ++ commandNameStr ++ " -> Maybe " ++ entityTypeStr ++ " -> Decision event",
            expectedSignatureWithoutUuid = "decide :: " ++ commandNameStr ++ " -> Maybe " ++ entityTypeStr ++ " -> Decision event"
          }

  validateFunctionSignature getEntityIdFuncInfo multiTenancyMode someName
  validateFunctionSignature decideFuncInfo multiTenancyMode someName

  let multiTenancyDecl = case maybeMultiTenancy of
        Just multiTenancyType ->
          [ TH.TySynInstD
              (TH.TySynEqn Nothing (TH.ConT (TH.mkName "IsMultiTenant") `TH.AppT` TH.ConT someName) (TH.ConT multiTenancyType))
          ]
        Nothing -> []

  let commandInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT commandClassName `TH.AppT` TH.ConT someName)
          ( multiTenancyDecl
              ++ [ TH.ValD (TH.VarP (TH.mkName "getEntityIdImpl")) (TH.NormalB (TH.VarE getEntityId)) [],
                   TH.ValD (TH.VarP (TH.mkName "decideImpl")) (TH.NormalB (TH.VarE decide)) []
                 ]
          )

  pure [commandInstance]
{-# INLINE deriveCommand #-}
