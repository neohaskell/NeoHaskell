{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Clippy (
  plugin,
) where

import Control.Exception (SomeException, tryJust)
import Data.Function
import Data.IORef
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.ICU (regex)
import Data.Text.ICU.Replace (replaceAll)
import Dhall hiding (map)
import GHC.Data.Bag (mapBag, unionBags, unitBag)
import GHC.Driver.Errors
import GHC.Plugins hiding (Rule, (<>))
import GHC.Tc.Plugin
import GHC.Tc.Types (TcLclEnv (tcl_errs), TcPlugin (..), TcPluginResult (TcPluginOk))
import GHC.Types.Error (mkMessages, partitionMessages)
import GHC.Utils.Error
import GHC.Utils.Logger (initLogger)
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
                tcPluginStop = const $ loadConfig >>= either cantInitializeWarning replaceMessages
              },
      pluginRecompile = purePlugin
    }


newtype Config = Config {rules :: [Rule]} deriving (Generic)


instance FromDhall Config


data Rule = Rule
  { match :: Text,
    print :: Text
  }
  deriving (Generic)


instance FromDhall Rule


data PEnv = PEnv
  { showMsgDoc :: SDoc -> Text,
    config :: Config,
    dynFlags :: DynFlags
  }


loadConfig :: TcPluginM (Either String Config)
loadConfig =
  tcPluginIO
    . tryJust (Just . show @SomeException)
    $ inputFile auto "./.clippy.dhall"


cantInitializeWarning :: String -> TcPluginM ()
cantInitializeWarning cause = do
  env <- getTopEnv
  let dynFlags = hsc_dflags env
  logger <- tcPluginIO initLogger
  let _span = mkGeneralSrcSpan $ mkFastString "ghc-clippy-plugin"
  let msgDoc = text "Clippy plugin couldn't start. Cause:" $$ text cause
  let warning = mkPlainWarnMsg _span msgDoc
  tcPluginIO $ printOrThrowWarnings logger dynFlags $ unitBag warning


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
          let warns' = mapBag replaceErrMsgs warns
          let errors' = mapBag replaceErrMsgs errors
          let newErrors = unionBags errors' warns'
          mkMessages newErrors
      )


-- (mkMessages . unitBag . replaceErrMsgs)

replaceErrSDoc :: PEnv -> MsgEnvelope DecoratedSDoc -> MsgEnvelope DecoratedSDoc
replaceErrSDoc env e =
  (replaceSDocs env) e


renderSDoc :: DynFlags -> SDoc -> Text
renderSDoc dynFlags sdoc = T.pack $ showSDoc dynFlags sdoc


-- replaceSDocText :: PEnv -> SDoc -> SDoc
-- replaceSDocText env sdoc =
--   let originalText = renderSDoc (dynFlags env) sdoc
--       replacedText = replaceText env originalText
--   in  text (T.unpack replacedText)

replaceSDocs :: PEnv -> MsgEnvelope DecoratedSDoc -> MsgEnvelope DecoratedSDoc
replaceSDocs env msgEnv = do
  let sDocList = unDecorated (errMsgDiagnostic msgEnv)
  let replacedDiagnostic = replaceSDocsGroup env "D" sDocList
  msgEnv {errMsgDiagnostic = mkDecorated replacedDiagnostic}


replaceSDocsGroup :: PEnv -> Text -> [SDoc] -> [SDoc]
replaceSDocsGroup env label msgDocs = text . T.unpack <$> filtered
 where
  filtered = filter (not . T.null . T.strip) (T.lines replaced)
  replaced = replaceText env wrapped
  wrapped = (env & showMsgDoc) . vcat $ wrapGroup label msgDocs


replaceText :: PEnv -> Text -> Text
replaceText env t = foldr replaceRule t (rules . config $ env)


replaceRule :: Rule -> Text -> Text
replaceRule rule = replaceAll (regex [] $ rule & match) (fromString . T.unpack $ rule & print)


wrapGroup :: Text -> [SDoc] -> [SDoc]
wrapGroup label group =
  [open $ ">" <> label] ++ (wrapDoc label <$> group) ++ [close $ label <> "<"]


wrapDoc :: Text -> SDoc -> SDoc
wrapDoc label doc = vcat [open label, doc, close label]


open :: Text -> SDoc
open label = text . T.unpack $ ">" <> label <> ">"


close :: Text -> SDoc
close label = text . T.unpack $ "<" <> label <> "<"
