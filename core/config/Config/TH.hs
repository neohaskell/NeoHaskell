{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Template Haskell macro for the NeoHaskell Config DSL.
--
-- This module provides the 'defineConfig' macro that generates:
--
-- 1. A data type with the specified fields
-- 2. A 'HasParser' instance for opt-env-conf integration
-- 3. A 'HasXxxConfig' type alias for implicit parameter access
module Config.TH (
  defineConfig,
) where

import Config.Core (ConfigError (..), FieldDef (..), FieldModifier (..), validateFieldDef)
import Control.Monad qualified as GhcMonad
import Control.Monad.Fail qualified as GhcFail
import Core
import Data.Char qualified as GhcChar
import Data.List qualified as GhcList
import Data.Maybe qualified as GhcMaybe
import Data.Text qualified as Text
import GHC.Generics qualified as GhcGenerics
import Language.Haskell.TH qualified as TH

import OptEnvConf qualified


-- | Define a configuration type with the given name and fields.
--
-- This macro generates:
--
-- * A data type: @data AppConfig = AppConfig { field1 :: Type1, ... }@
-- * A HasParser instance using opt-env-conf
-- * A type alias: @type HasAppConfig = (?config :: AppConfig)@
--
-- All fields must have 'Config.doc' and either 'Config.defaultsTo' or 'Config.required'.
defineConfig :: Text -> [FieldDef] -> TH.Q [TH.Dec]
defineConfig configNameText fields = do
  -- Validate all fields at compile time
  validateAllFields fields

  let configName = TH.mkName (Text.unpack configNameText)
  let hasConfigName = TH.mkName ("Has" ++ Text.unpack configNameText)

  -- Generate data type
  dataDecl <- generateDataType configName fields

  -- Generate HasParser instance
  parserDecl <- generateHasParser configName fields

  -- Generate type alias: type HasAppConfig = (?config :: AppConfig)
  let typeAlias = TH.TySynD hasConfigName [] (implicitParamType configName)

  return [dataDecl, parserDecl, typeAlias]


-- | Validate all fields and fail compilation if any errors found.
validateAllFields :: [FieldDef] -> TH.Q ()
validateAllFields fields = do
  let allErrors = GhcList.concatMap validateFieldDef fields
  GhcMonad.unless (GhcList.null allErrors) do
    let errorMsg = allErrors |> GhcList.map (\err -> Text.unpack (formatError err)) |> GhcList.unlines
    GhcFail.fail errorMsg


formatError :: ConfigError -> Text
formatError err =
  case err of
    MissingDoc fieldName ->
      [fmt|Config field '#{fieldName}' is missing Config.doc. All fields must be documented.|]
    MissingDefaultOrRequired fieldName ->
      [fmt|Config field '#{fieldName}' must have either Config.defaultsTo or Config.required.|]
    BothDefaultAndRequired fieldName ->
      [fmt|Config field '#{fieldName}' has both Config.defaultsTo and Config.required. Choose one.|]


-- | Generate: data AppConfig = AppConfig { field1 :: Type1, ... }
generateDataType :: TH.Name -> [FieldDef] -> TH.Q TH.Dec
generateDataType configName fields = do
  let recordFields = GhcList.map fieldToVarBangType fields
  let constructor = TH.RecC configName recordFields
  let deriveClauses =
        [ TH.DerivClause Nothing [TH.ConT ''Show, TH.ConT ''GhcGenerics.Generic]
        ]
  return (TH.DataD [] configName [] Nothing [constructor] deriveClauses)


fieldToVarBangType :: FieldDef -> TH.VarBangType
fieldToVarBangType fd = do
  let name = TH.mkName (Text.unpack fd.fieldName)
  let bang = TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
  (name, bang, fd.fieldType)


-- | Generate: instance HasParser AppConfig where settingsParser = ...
generateHasParser :: TH.Name -> [FieldDef] -> TH.Q TH.Dec
generateHasParser configName fields = do
  -- Build: AppConfig <$> setting1 <*> setting2 <*> ...
  parserBody <- buildParserBody configName fields

  let settingsParserDef =
        TH.FunD
          'OptEnvConf.settingsParser
          [TH.Clause [] (TH.NormalB parserBody) []]

  return
    ( TH.InstanceD
        Nothing
        []
        (TH.ConT ''OptEnvConf.HasParser `TH.AppT` TH.ConT configName)
        [settingsParserDef]
    )


-- | Build: ConfigName <$> setting [...] <*> setting [...] <*> ...
buildParserBody :: TH.Name -> [FieldDef] -> TH.Q TH.Exp
buildParserBody configName fields =
  case fields of
    [] ->
      -- No fields: pure ConfigName
      [|pure $(TH.conE configName)|]
    (firstField : restFields) -> do
      -- Start with: ConfigName <$> firstSetting
      firstSetting <- fieldToSettingExp firstField
      let fmapVar = TH.VarE 'fmap
      let initial = TH.InfixE (Just (TH.ConE configName)) fmapVar (Just firstSetting)

      -- Fold remaining: <*> setting2 <*> setting3 ...
      GhcMonad.foldM addSettingToParser initial restFields


addSettingToParser :: TH.Exp -> FieldDef -> TH.Q TH.Exp
addSettingToParser acc fd = do
  settingExp <- fieldToSettingExp fd
  let apVar = TH.VarE '(<*>)
  return (TH.InfixE (Just acc) apVar (Just settingExp))


-- | Convert a FieldDef to an opt-env-conf setting expression.
fieldToSettingExp :: FieldDef -> TH.Q TH.Exp
fieldToSettingExp fd = do
  builders <- fieldModifiersToBuilders fd
  -- Generate: setting [builder1, builder2, ..., reader auto]
  readerExp <- [|OptEnvConf.reader OptEnvConf.auto|]
  let allBuilders = builders ++ [readerExp]
  let builderListExp = TH.ListE allBuilders
  return (TH.AppE (TH.VarE 'OptEnvConf.setting) builderListExp)


-- | Convert field modifiers to opt-env-conf builder expressions.
fieldModifiersToBuilders :: FieldDef -> TH.Q [TH.Exp]
fieldModifiersToBuilders fd = do
  let mods = fd.fieldModifiers
  let name = fd.fieldName

  -- Always add env var (auto-generate if not specified)
  envVarExp <- case findEnvVar mods of
    Just var -> [|OptEnvConf.env $(TH.litE (TH.stringL (Text.unpack var)))|]
    Nothing -> [|OptEnvConf.env $(TH.litE (TH.stringL (Text.unpack (toEnvVarName name))))|]

  -- Collect all builders
  docExps <- GhcMonad.sequence (GhcMaybe.mapMaybe modToDoc mods)
  defaultExps <- GhcMonad.sequence (GhcMaybe.mapMaybe modToDefault mods)
  cliLongExps <- GhcMonad.sequence (GhcMaybe.mapMaybe modToCliLong mods)
  cliShortExps <- GhcMonad.sequence (GhcMaybe.mapMaybe modToCliShort mods)

  return (docExps ++ [envVarExp] ++ defaultExps ++ cliLongExps ++ cliShortExps)


findEnvVar :: [FieldModifier] -> Maybe Text
findEnvVar mods =
  mods
    |> GhcList.find isEnvVar
    |> fmap extractEnvVar
 where
  isEnvVar m = case m of
    ModEnvVar _ -> True
    _ -> False
  extractEnvVar m = case m of
    ModEnvVar v -> v
    _ -> ""


modToDoc :: FieldModifier -> Maybe (TH.Q TH.Exp)
modToDoc modifier =
  case modifier of
    ModDoc txt ->
      Just [|OptEnvConf.help $(TH.litE (TH.stringL (Text.unpack txt)))|]
    _ ->
      Nothing


modToDefault :: FieldModifier -> Maybe (TH.Q TH.Exp)
modToDefault modifier =
  case modifier of
    ModDefault qExpr -> do
      -- qExpr is already a Q Exp, we need to sequence it and wrap with OptEnvConf.value
      Just do
        expr <- qExpr
        return (TH.AppE (TH.VarE 'OptEnvConf.value) expr)
    _ ->
      Nothing


modToCliLong :: FieldModifier -> Maybe (TH.Q TH.Exp)
modToCliLong modifier =
  case modifier of
    ModCliLong name ->
      Just [|OptEnvConf.long $(TH.litE (TH.stringL (Text.unpack name)))|]
    _ ->
      Nothing


modToCliShort :: FieldModifier -> Maybe (TH.Q TH.Exp)
modToCliShort modifier =
  case modifier of
    ModCliShort char ->
      Just [|OptEnvConf.short $(TH.litE (TH.charL char))|]
    _ ->
      Nothing


-- | Convert camelCase field name to SCREAMING_SNAKE_CASE env var name.
--
-- Examples:
-- * @port@ -> @PORT@
-- * @databaseUrl@ -> @DATABASE_URL@
-- * @openRouterKey@ -> @OPEN_ROUTER_KEY@
toEnvVarName :: Text -> Text
toEnvVarName name = do
  let chars = Text.unpack name
  let converted = insertUnderscores chars
  Text.toUpper (Text.pack converted)
 where
  insertUnderscores :: [Char] -> [Char]
  insertUnderscores [] = []
  insertUnderscores [c] = [c]
  insertUnderscores (c1 : c2 : rest) =
    case (GhcChar.isLower c1, GhcChar.isUpper c2) of
      (True, True) ->
        c1 : '_' : insertUnderscores (c2 : rest)
      _ ->
        c1 : insertUnderscores (c2 : rest)


-- | Generate type for implicit parameter: (?config :: AppConfig)
implicitParamType :: TH.Name -> TH.Type
implicitParamType configName =
  TH.ImplicitParamT "config" (TH.ConT configName)
