{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Clippy (
  plugin,
) where

import Clippy.Rules qualified as Rules
import Data.Function
import Data.IORef
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.ICU (regex)
import Data.Text.ICU.Replace (replaceAll)
import GHC.Data.Bag (mapBag, unionBags)
import GHC.Generics (Generic)
import GHC.Plugins hiding (Rule, (<>))
import GHC.Tc.Plugin
import GHC.Tc.Types (TcLclEnv (tcl_errs), TcPlugin (..), TcPluginResult (TcPluginOk))
import GHC.Types.Error (mkMessages, partitionMessages)
import GHC.Utils.Error
import Prelude hiding (print)


plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin =
        const $
          Just $
            TcPlugin
              { tcPluginInit = pure (),
                tcPluginSolve = \_ _ _ _ -> pure $ TcPluginOk [] [],
                tcPluginStop = const $ loadConfig >>= replaceMessages
              },
      pluginRecompile = purePlugin
    }


newtype Config = Config {rules :: [Rules.Rule]} deriving (Generic)


data PEnv = PEnv
  { showMsgDoc :: SDoc -> Text,
    config :: Config,
    dynFlags :: DynFlags
  }


loadConfig :: TcPluginM Config
loadConfig =
  pure
    (Config Rules.rules)


replaceMessages :: Config -> TcPluginM ()
replaceMessages conf = do
  dynFlags <- hsc_dflags <$> getTopEnv
  let env = PEnv {showMsgDoc = renderSDoc dynFlags, config = conf, dynFlags = dynFlags}
  errsRef <- tcl_errs . snd <$> getEnvs
  let _showSDoc = T.pack . showSDoc dynFlags
  let replaceErrMsgs = replaceErrSDoc env
  tcPluginIO $
    modifyIORef
      errsRef
      ( \msgs -> do
          let (warns, errors) = partitionMessages msgs
          let warns' = mapBag (replaceErrMsgs "W") warns
          let errors' = mapBag (replaceErrMsgs "E") errors
          let newErrors = unionBags errors' warns'
          mkMessages newErrors
      )


replaceErrSDoc :: PEnv -> Text -> MsgEnvelope DecoratedSDoc -> MsgEnvelope DecoratedSDoc
replaceErrSDoc env nametype e =
  (replaceSDocs env nametype) e


renderSDoc :: DynFlags -> SDoc -> Text
renderSDoc dynFlags sdoc = T.pack $ showSDoc dynFlags sdoc


-- replaceSDocText :: PEnv -> SDoc -> SDoc
-- replaceSDocText env sdoc =
--   let originalText = renderSDoc (dynFlags env) sdoc
--       replacedText = replaceText env originalText
--   in  text (T.unpack replacedText)

replaceSDocs :: PEnv -> Text -> MsgEnvelope DecoratedSDoc -> MsgEnvelope DecoratedSDoc
replaceSDocs env nametype msgEnv = do
  let sDocList = unDecorated (errMsgDiagnostic msgEnv)
  -- let msgSpan = errMsgSpan msgEnv
  let replacedDiagnostic = replaceSDocsGroup env nametype sDocList
  msgEnv {errMsgDiagnostic = mkDecorated replacedDiagnostic}


replaceSDocsGroup :: PEnv -> Text -> [SDoc] -> [SDoc]
replaceSDocsGroup env label msgDocs = text . T.unpack <$> filtered
 where
  filtered = filter (not . T.null . T.strip) (T.lines replaced)
  replaced = replaceText env wrapped
  wrapped = (env & showMsgDoc) . vcat $ wrapGroup label msgDocs


replaceText :: PEnv -> Text -> Text
replaceText env t = foldr replaceRule t (rules . config $ env)


replaceRule :: Rules.Rule -> Text -> Text
replaceRule rule = replaceAll (regex [] $ rule & Rules.match) (fromString . T.unpack $ rule & Rules.print)


wrapGroup :: Text -> [SDoc] -> [SDoc]
wrapGroup label group =
  [open $ ">" <> label] ++ (wrapDoc label <$> group) ++ [close $ label <> "<"]


wrapDoc :: Text -> SDoc -> SDoc
wrapDoc label doc = vcat [open label, doc, close label]


open :: Text -> SDoc
open label = text . T.unpack $ ">" <> label <> ">"


close :: Text -> SDoc
close label = text . T.unpack $ "<" <> label <> "<"
