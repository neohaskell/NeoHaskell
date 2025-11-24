{-# LANGUAGE AllowAmbiguousTypes #-}

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
  let modeDescription = case mode of
        MustHaveUuid ->
          "When MultiTenancy is set to True"
        MustNotHaveUuid ->
          "When MultiTenancy is False or undefined"
  let additionalHelp = case mode of
        MustHaveUuid ->
          "Please update your " ++ functionName ++ " function to accept a Uuid tenant ID as the first argument."
        MustNotHaveUuid ->
          "The function should NOT have Uuid as a parameter when MultiTenancy is False.\nIf you need multi-tenancy, define: type MultiTenancy = True"
  "ERROR: '"
    ++ functionName
    ++ "' has incorrect signature.\n\n"
    ++ modeDescription
    ++ ":\nExpected signature:\n  "
    ++ expectedSig
    ++ "\n\nCurrent signature:\n  "
    ++ currentSig
    ++ "\n\n"
    ++ additionalHelp


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

  maybeEntityType <- TH.lookupTypeName "EntityOf"
  maybeGetEntityId <- TH.lookupValueName "getEntityId"
  maybeDecide <- TH.lookupValueName "decide"
  entityType <- maybeEntityType |> orError "FIXME: This module doesn't have an Entity type"
  getEntityId <- maybeGetEntityId |> orError "FIXME: This module doesn't have a getEntityId function"
  decide <- maybeDecide |> orError "FIXME: This module doesn't have a decide function"

  maybeMultiTenancy <- TH.lookupTypeName "MultiTenancy"
  commandClassName <- TH.lookupTypeName "Command" >>= orError "FIXME: Command type class not found"

  multiTenancyMode <- determineMultiTenancyMode maybeMultiTenancy

  let commandNameStr = TH.nameBase someName
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
