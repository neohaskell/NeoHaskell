-- | # Service.InboundIntegration.TH
--
-- Derive a typed inbound handler from its module's @workerImpl@ signature.
-- Call 'inboundIntegration' after the worker implementation and its command types.
module Service.InboundIntegration.TH (
  inboundIntegration,
) where

import Appendable ((++))
import Basics
import Control.Monad.Fail qualified as MonadFail
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Ppr qualified as THPpr
import Language.Haskell.TH.Syntax qualified as TH
import Maybe (Maybe (..))
import Service.CommandExecutor.TH (deriveKnownHash)
import Text (Text)
import Text qualified


-- | Derive 'InboundIntegration', 'NameOf', and the handler hash identity.
--
-- @workerImpl :: Config -> Integration.TypedInbound '[ '(Command, Entity)]@
-- must be in scope. The output list is copied into 'InboundOutputsOf' so the
-- Event Model can check every command and its target.
inboundIntegration :: TH.Name -> THLib.DecsQ
inboundIntegration handlerName = do
  workerName <- TH.lookupValueName "workerImpl"
    >>= required "Missing workerImpl. Define workerImpl :: Config -> Integration.TypedInbound Outputs before inboundIntegration."
  workerInfo <- TH.reify workerName
  (configType, outputsType) <- case workerInfo of
    TH.VarI _ signature _ -> parseWorkerSignature signature
    _ -> MonadFail.fail "Could not inspect workerImpl signature."

  className <- lookupType "InboundIntegration"
  configFamilyName <- lookupType "InboundConfigOf"
  outputsFamilyName <- lookupType "InboundOutputsOf"
  nameFamilyName <- lookupType "NameOf"
  let handlerType = TH.ConT handlerName
  let instanceDeclaration =
        TH.InstanceD Nothing [] (TH.ConT className `TH.AppT` handlerType)
          [ TH.TySynInstD (TH.TySynEqn Nothing (TH.ConT configFamilyName `TH.AppT` handlerType) configType)
          , TH.TySynInstD (TH.TySynEqn Nothing (TH.ConT outputsFamilyName `TH.AppT` handlerType) outputsType)
          , TH.ValD (TH.VarP (TH.mkName "workerImplFor")) (TH.NormalB (TH.VarE workerName)) []
          ]
  let handlerNameText = TH.nameBase handlerName
  let nameDeclaration =
        TH.TySynInstD
          (TH.TySynEqn Nothing (TH.ConT nameFamilyName `TH.AppT` handlerType) (TH.LitT (TH.StrTyLit handlerNameText)))
  hashDeclarations <- deriveKnownHash handlerNameText
  pure ([nameDeclaration, instanceDeclaration] ++ hashDeclarations)


parseWorkerSignature :: TH.Type -> TH.Q (TH.Type, TH.Type)
parseWorkerSignature signature = case signature of
  TH.ForallT _ _ inner -> parseWorkerSignature inner
  TH.SigT inner _ -> parseWorkerSignature inner
  TH.AppT (TH.AppT TH.ArrowT configType) (TH.AppT (TH.ConT inboundType) outputsType)
    | TH.nameBase inboundType == "TypedInbound" -> pure (configType, outputsType)
  _ -> MonadFail.fail [fmt|workerImpl must have type Config -> Integration.TypedInbound Outputs. Found: #{THPpr.pprint signature}|]


lookupType :: Text -> TH.Q TH.Name
lookupType name = do
  result <- TH.lookupTypeName (Text.toLinkedList name)
  required [fmt|Missing #{name}. Import Core before inboundIntegration.|] result


required :: Text -> Maybe value -> TH.Q value
required message result = case result of
  Just value -> pure value
  Nothing -> MonadFail.fail (Text.toLinkedList message)
