module Service.CommandHandler.TH (
  deriveCommand,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
import Data.Hashable qualified as Hashable
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
  -- Split by "->" to get parts of the function signature
  let parts = typeStr |> splitOn "->"
  -- Check if Uuid appears in any part BEFORE the last part (which is the return type)
  -- We want to check if Uuid is a parameter, not in the return type
  case parts of
    [] -> False
    [_] -> False -- No arrows, so no parameters
    _ -> do
      -- Get all parts except the last one (return type)
      let parameterParts = parts |> GhcList.init
      -- Check if any parameter part contains "Uuid"
      parameterParts
        |> GhcList.any (\part -> "Uuid" `isInfixOf` part)


-- Helper function to split string by a delimiter
splitOn :: String -> String -> [String]
splitOn delimiter str = do
  let go acc current remaining =
        case remaining of
          [] -> GhcList.reverse (GhcList.reverse current : acc)
          c : rest -> do
            if GhcList.take (GhcList.length delimiter) remaining == delimiter
              then go (GhcList.reverse current : acc) [] (GhcList.drop (GhcList.length delimiter) remaining)
              else go acc (c : current) rest
  go [] [] str


-- Helper function to check if a string is an infix of another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = do
  let go [] _ = True
      go _ [] = False
      go n h@(_ : hs) =
        (GhcList.take (GhcList.length n) h == n) || go n hs
  go needle haystack


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

  maybeGetEntityId <- TH.lookupValueName "getEntityId"
  maybeDecide <- TH.lookupValueName "decide"

  -- Try to resolve the EntityOf type instance for this command
  -- We use reifyInstances to check if there's a type instance defined
  entityTypeFamilyName <- TH.lookupTypeName "EntityOf"
  entityTypeInstances <-
    case entityTypeFamilyName of
      Nothing ->
        MonadFail.fail
          [fmt|
ERROR: EntityOf type family not found.

Please ensure you have `import Core` at the top of your module.
|]
      Just typeFamilyName -> TH.reifyInstances typeFamilyName [TH.ConT someName]

  entityType <-
    case entityTypeInstances of
      [] ->
        MonadFail.fail
          [fmt|
ERROR: Missing EntityOf type instance for command '#{commandNameStr}'.

Commands need to specify which entity type they operate on. This allows the command
handler to fetch the current state of the entity before deciding whether to accept
or reject the command.

Please add the following type instance to your module:

  type instance EntityOf #{commandNameStr} = YourEntityType

For more information on what commands and entities are, take a look at the docs: FIXME: ADD DOCS
|]
      (TH.TySynInstD (TH.TySynEqn _ _ entityTypeExpr) : _) ->
        case entityTypeExpr of
          TH.ConT name -> pure name
          _ ->
            MonadFail.fail
              [fmt|
ERROR: EntityOf type instance for '#{commandNameStr}' must be a concrete type.

The EntityOf type instance should map to a specific entity type, not a type variable
or complex type expression.

Current definition resolves to: #{THPpr.pprint entityTypeExpr}

Please ensure your type instance uses a concrete entity type:

  type instance EntityOf #{commandNameStr} = YourEntityType
|]
      _ ->
        MonadFail.fail
          [fmt|
ERROR: Unexpected EntityOf type instance format for '#{commandNameStr}'.

Please ensure your type instance follows this format:

  type instance EntityOf #{commandNameStr} = YourEntityType
|]

  -- Try to resolve the EntityIdType type instance for this command
  -- EntityIdType is an associated type family in the Command class
  -- Look up EntityIdType associated type family
  entityIdTypeFamilyName <- TH.lookupTypeName "EntityIdType"

  entityIdTypeInstances <-
    case entityIdTypeFamilyName of
      Nothing ->
        -- If we can't find EntityIdType, default to Uuid
        pure []
      Just typeFamilyName -> TH.reifyInstances typeFamilyName [TH.ConT someName]

  entityIdType <-
    case entityIdTypeInstances of
      [] -> do
        -- No instance defined, use the default type Uuid
        maybeUuidType <- TH.lookupTypeName "Uuid"
        case maybeUuidType of
          Just uuidType -> pure uuidType
          Nothing ->
            MonadFail.fail
              [fmt|
ERROR: Uuid type not found and no EntityIdType instance defined.

Please ensure you have `import Core` at the top of your module.
|]
      (TH.TySynInstD (TH.TySynEqn _ _ entityIdTypeExpr) : _) ->
        case entityIdTypeExpr of
          TH.ConT name -> pure name
          _ ->
            MonadFail.fail
              [fmt|
ERROR: EntityIdType instance for '#{commandNameStr}' must be a concrete type.

The EntityIdType should be a specific type, not a type variable or complex type expression.

Current definition resolves to: #{THPpr.pprint entityIdTypeExpr}

Please ensure your EntityIdType uses a concrete type like Uuid or Text.
|]
      _ ->
        MonadFail.fail
          [fmt|
ERROR: Unexpected EntityIdType instance format for '#{commandNameStr}'.
|]

  let entityIdTypeStr = TH.nameBase entityIdType

  getEntityId <-
    maybeGetEntityId
      |> orError
        [fmt|
ERROR: Missing 'getEntityId' function for command '#{commandNameStr}'.

The 'getEntityId' function extracts the entity identifier from your command.
This ID is used to fetch the current state of the entity from the event store.

Please add the following function to your module:

  getEntityId :: #{commandNameStr} -> Maybe #{entityIdTypeStr}
  getEntityId command = ...

If your command creates a new entity (no existing ID), return Nothing:
  getEntityId _ = Nothing


For more information on what commands and entities are, take a look at the docs: FIXME: ADD DOCS
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
      Just _ -> Decision.reject "Cart already exists"
      Nothing -> Decision.acceptNew [CartCreated]

  -- For updating an existing entity:
  decide cmd entity = do
    case entity of
      Nothing -> Decision.reject "Cart not found"
      Just existingEntity -> Decision.acceptExisting [CartRenamed]

For more information on what commands and entities are, take a look at the docs: FIXME: ADD DOCS
|]

  maybeMultiTenancy <- TH.lookupTypeName "MultiTenancy"
  multiTenancyMode <- determineMultiTenancyMode maybeMultiTenancy

  let entityTypeStr = TH.nameBase entityType

  let getEntityIdFuncInfo =
        FunctionInfo
          { functionName = "getEntityId",
            thName = getEntityId,
            expectedSignatureWithUuid = "getEntityId :: Uuid -> " ++ commandNameStr ++ " -> Maybe " ++ entityIdTypeStr,
            expectedSignatureWithoutUuid = "getEntityId :: " ++ commandNameStr ++ " -> Maybe " ++ entityIdTypeStr
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

  -- Generate NameOf type instance
  nameOfTypeFamilyName <-
    TH.lookupTypeName "NameOf"
      >>= orError
        [fmt|
ERROR: NameOf type family not found.

Please ensure you have `import Core` at the top of your module.
|]

  -- Generate KnownHash instance for the command name
  knownHashClassName <-
    TH.lookupTypeName "KnownHash"
      >>= orError
        [fmt|
ERROR: KnownHash type class not found.

Please ensure you have `import Core` at the top of your module.
|]

  -- Lookup Command class for instance generation
  commandClassName <-
    TH.lookupTypeName "Command"
      >>= orError
        [fmt|
ERROR: Command type class not found.

Please ensure you have `import Core` at the top of your module.
|]

  let commandNameLit = TH.LitT (TH.StrTyLit commandNameStr)

  -- Compute hash at compile time using hashable
  let commandHash = Hashable.hash commandNameStr

  -- Generate: type instance NameOf CommandName = "CommandName"
  let nameOfInstance =
        TH.TySynInstD
          (TH.TySynEqn Nothing (TH.ConT nameOfTypeFamilyName `TH.AppT` TH.ConT someName) commandNameLit)

  -- Generate: instance KnownHash "CommandName" where
  --             hashVal _ = <computed hash>
  let knownHashInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT knownHashClassName `TH.AppT` commandNameLit)
          [ TH.FunD
              (TH.mkName "hashVal")
              [ TH.Clause
                  [TH.WildP]
                  (TH.NormalB (TH.LitE (TH.IntegerL (fromIntegral commandHash))))
                  []
              ]
          ]

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

  pure [nameOfInstance, knownHashInstance, commandInstance]
{-# INLINE deriveCommand #-}
