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
  -- * Testing utilities (exported for test coverage)
  toEnvVarName,
) where

import Config.Core (ConfigError (..), FieldDef (..), FieldModifier (..), validateFieldDef)
import Control.Monad qualified as GhcMonad
import Control.Monad.Fail qualified as GhcFail
import Core
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey

import Data.Char qualified as GhcChar
import Data.Default qualified as GhcDefault
import Data.Maybe qualified as GhcMaybe
import Data.Text qualified as Text
import LinkedList qualified
import GHC.Err qualified as GhcErr
import GHC.Generics qualified as GhcGenerics
import Language.Haskell.TH qualified as TH

import OptEnvConf qualified


-- | Define a configuration type with the given name and fields.
--
-- This macro generates:
--
-- * A data type: @data AppConfig = AppConfig { field1 :: Type1, ... }@
-- * A custom Show instance that redacts secret fields
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

  -- Generate data type (without deriving Show - we generate it manually)
  dataDecl <- generateDataType configName fields

  -- Generate custom Show instance that redacts secret fields
  showDecl <- generateShowInstance configName fields

  -- Generate Default instance
  defaultDecl <- generateDefaultInstance configName fields

  -- Generate FromJSON instance
  fromJsonDecl <- generateFromJsonInstance configName fields

  -- Generate ToJSON instance (with secret redaction)
  toJsonDecl <- generateToJsonInstance configName fields

  -- Generate HasParser instance
  parserDecl <- generateHasParser configName fields

  -- Generate type alias: type HasAppConfig = (?config :: AppConfig)
  let typeAlias = TH.TySynD hasConfigName [] (implicitParamType configName)

  return [dataDecl, showDecl, defaultDecl, fromJsonDecl, toJsonDecl, parserDecl, typeAlias]


-- | Validate all fields and fail compilation if any errors found.
validateAllFields :: [FieldDef] -> TH.Q ()
validateAllFields fields = do
  let allErrors = LinkedList.concatMap validateFieldDef fields
  GhcMonad.unless (LinkedList.null allErrors) do
    let errorMsg = allErrors |> LinkedList.map (\err -> Text.unpack (formatError err)) |> LinkedList.unlines
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
-- Note: Show is NOT derived - we generate a custom instance that redacts secrets
generateDataType :: TH.Name -> [FieldDef] -> TH.Q TH.Dec
generateDataType configName fields = do
  let recordFields = LinkedList.map fieldToVarBangType fields
  let constructor = TH.RecC configName recordFields
  let deriveClauses =
        [ TH.DerivClause Nothing [TH.ConT ''GhcGenerics.Generic]
        ]
  return (TH.DataD [] configName [] Nothing [constructor] deriveClauses)


fieldToVarBangType :: FieldDef -> TH.VarBangType
fieldToVarBangType fd = do
  let name = TH.mkName (Text.unpack fd.fieldName)
  let bang = TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
  (name, bang, fd.fieldType)


-- | Generate a custom Show instance that redacts secret fields.
--
-- For a config like:
-- @
-- defineConfig "AppConfig"
--   [ field @Int "port" |> doc "Port" |> defaultsTo 8080
--   , field @Text "apiKey" |> doc "API Key" |> secret |> required
--   ]
-- @
--
-- Generates:
-- @
-- instance Show AppConfig where
--   show config = "AppConfig {port = " ++ show config.port
--                 ++ ", apiKey = \"<REDACTED>\"}"
-- @
generateShowInstance :: TH.Name -> [FieldDef] -> TH.Q TH.Dec
generateShowInstance configName fields = do
  -- Create a variable for the config parameter
  configVar <- TH.newName "config"

  -- Build the show body
  showBody <- buildShowBody configName configVar fields

  -- Create the function clause: show config = ...
  let showClause = TH.Clause [TH.VarP configVar] (TH.NormalB showBody) []

  -- Create the instance declaration
  return
    ( TH.InstanceD
        Nothing
        []
        (TH.ConT ''Show `TH.AppT` TH.ConT configName)
        [TH.FunD 'show [showClause]]
    )


-- | Build the body of the show function.
-- Generates: "ConfigName {field1 = " ++ showField1 ++ ", field2 = " ++ showField2 ++ "}"
buildShowBody :: TH.Name -> TH.Name -> [FieldDef] -> TH.Q TH.Exp
buildShowBody configName configVar fields = do
  let configNameStr = TH.nameBase configName
  case fields of
    [] ->
      -- No fields: just return the config name
      return (TH.LitE (TH.StringL configNameStr))
    _ -> do
      -- Build field show expressions
      fieldExprs <- GhcMonad.mapM (buildFieldShowExpr configVar) fields

      -- Start with "ConfigName {"
      let prefix = TH.LitE (TH.StringL (configNameStr ++ " {"))

      -- Join field expressions with ", "
      let joinedFields = intersperseLit ", " fieldExprs

      -- End with "}"
      let suffix = TH.LitE (TH.StringL "}")

      -- Concatenate: prefix ++ field1 ++ ", " ++ field2 ++ ... ++ suffix
      let allParts = [prefix] ++ joinedFields ++ [suffix]
      return (concatStrings allParts)


-- | Build the show expression for a single field.
-- For secret fields: "fieldName = \"<REDACTED>\""
-- For normal fields: "fieldName = " ++ show config.fieldName
buildFieldShowExpr :: TH.Name -> FieldDef -> TH.Q TH.Exp
buildFieldShowExpr configVar fd = do
  let fieldNameStr = Text.unpack fd.fieldName
  let isSecretField = hasSecretModifier fd.fieldModifiers

  case isSecretField of
    True -> do
      -- Secret field: return literal "<REDACTED>"
      return (TH.LitE (TH.StringL (fieldNameStr ++ " = \"<REDACTED>\"")))
    False -> do
      -- Normal field: "fieldName = " ++ show (config.fieldName)
      let prefixLit = TH.LitE (TH.StringL (fieldNameStr ++ " = "))
      -- Access the field: config.fieldName (using GetFieldE for record dot syntax)
      let fieldAccess = TH.GetFieldE (TH.VarE configVar) fieldNameStr
      -- Apply show: show (config.fieldName)
      let showExpr = TH.AppE (TH.VarE 'show) fieldAccess
      -- Concatenate: "fieldName = " ++ show (config.fieldName)
      return (concatStrings [prefixLit, showExpr])


-- | Check if a field has the ModSecret modifier.
hasSecretModifier :: [FieldModifier] -> Bool
hasSecretModifier mods =
  LinkedList.any isSecret mods
 where
  isSecret m = case m of
    ModSecret -> True
    _ -> False


-- | Check if a field has the ModNested modifier.
hasNestedModifier :: [FieldModifier] -> Bool
hasNestedModifier mods =
  LinkedList.any isNested mods
 where
  isNested m = case m of
    ModNested -> True
    _ -> False


-- | Check if a TH type is Text or String.
--
-- These types need the 'str' reader (IsString-based) instead of 'auto' (Read-based)
-- because Read for strings expects quoted values like @"hello"@, but environment
-- variables contain raw unquoted strings.
isTextLikeType :: TH.Type -> Bool
isTextLikeType thType =
  case thType of
    TH.ConT name -> do
      let nameStr = TH.nameBase name
      nameStr == "Text" || nameStr == "String"
    _ -> False


-- | Find the ModEnvPrefix value if present.
findEnvPrefix :: [FieldModifier] -> Maybe Text
findEnvPrefix mods =
  mods
    |> LinkedList.find isEnvPrefix
    |> fmap extractEnvPrefix
 where
  isEnvPrefix m = case m of
    ModEnvPrefix _ -> True
    _ -> False
  extractEnvPrefix m = case m of
    ModEnvPrefix p -> p
    _ -> ""


-- | Intersperse a separator literal between expressions.
-- [a, b, c] with sep -> [a, sep, b, sep, c]
intersperseLit :: [Char] -> [TH.Exp] -> [TH.Exp]
intersperseLit _ [] = []
intersperseLit _ [x] = [x]
intersperseLit sep (x : xs) =
  x : TH.LitE (TH.StringL sep) : intersperseLit sep xs


-- | Concatenate a list of string expressions using (++).
concatStrings :: [TH.Exp] -> TH.Exp
concatStrings [] = TH.LitE (TH.StringL "")
concatStrings [x] = x
concatStrings (x : xs) =
  TH.InfixE (Just x) (TH.VarE '(++)) (Just (concatStrings xs))


-- | Generate: instance Default AppConfig where def = AppConfig { ... }
--
-- For fields with 'defaultsTo': uses the provided default value
-- For 'required' fields: uses @error "Required field: fieldName"@
--
-- This allows testing with partial configs using record update syntax:
-- @
-- let testConfig = def { port = 3000 }
-- @
generateDefaultInstance :: TH.Name -> [FieldDef] -> TH.Q TH.Dec
generateDefaultInstance configName fields = do
  -- Build field assignments for the record
  fieldAssigns <- GhcMonad.mapM fieldToDefaultAssign fields

  -- Build: AppConfig { field1 = value1, field2 = value2, ... }
  let recordExp = TH.RecConE configName fieldAssigns

  -- Create: def = AppConfig { ... }
  let defClause = TH.Clause [] (TH.NormalB recordExp) []

  return
    ( TH.InstanceD
        Nothing
        []
        (TH.ConT ''GhcDefault.Default `TH.AppT` TH.ConT configName)
        [TH.FunD 'GhcDefault.def [defClause]]
    )


-- | Generate the default value for a single field.
--
-- - Fields with 'defaultsTo': use the provided value
-- - Nested fields: use @def@ from the nested type's Default instance
-- - Required fields: use @error "Required field: fieldName"@
fieldToDefaultAssign :: FieldDef -> TH.Q TH.FieldExp
fieldToDefaultAssign fd = do
  let fieldName = TH.mkName (Text.unpack fd.fieldName)
  let mods = fd.fieldModifiers
  let isNestedField = hasNestedModifier mods

  -- Find the default value if present
  defaultValue <- case findDefault mods of
    Just qExpr -> qExpr
    Nothing -> case isNestedField of
      True -> do
        -- Nested field - use def from the nested type
        return (TH.VarE 'GhcDefault.def)
      False -> do
        -- Required field - use error
        let errMsg = "Required field: " ++ Text.unpack fd.fieldName
        return (TH.AppE (TH.VarE 'GhcErr.error) (TH.LitE (TH.StringL errMsg)))

  return (fieldName, defaultValue)


-- | Find the ModDefault in the modifiers and extract its Q Exp.
findDefault :: [FieldModifier] -> Maybe (TH.Q TH.Exp)
findDefault mods =
  mods
    |> GhcMaybe.mapMaybe extractDefault
    |> GhcMaybe.listToMaybe
 where
  extractDefault m = case m of
    ModDefault qExpr -> Just qExpr
    _ -> Nothing


-- | Generate: instance FromJSON AppConfig where parseJSON = ...
--
-- Generates a standard FromJSON instance that parses all fields.
-- Safe for secrets because parsed values become protected immediately
-- if wrapped in Redacted.
generateFromJsonInstance :: TH.Name -> [FieldDef] -> TH.Q TH.Dec
generateFromJsonInstance configName fields = do
  -- Create parameter for the JSON value
  jsonVar <- TH.newName "v"

  -- Build the parsing body
  parseBody <- buildFromJsonBody configName fields jsonVar

  -- Create: parseJSON v = withObject "ConfigName" $ \obj -> ...
  let parseClause = TH.Clause [TH.VarP jsonVar] (TH.NormalB parseBody) []

  return
    ( TH.InstanceD
        Nothing
        []
        (TH.ConT ''Aeson.FromJSON `TH.AppT` TH.ConT configName)
        [TH.FunD 'Aeson.parseJSON [parseClause]]
    )


-- | Build the body of the parseJSON function.
-- Generates: withObject "ConfigName" $ \obj -> ConfigName <$> obj .: "field1" <*> ...
buildFromJsonBody :: TH.Name -> [FieldDef] -> TH.Name -> TH.Q TH.Exp
buildFromJsonBody configName fields jsonVar = do
  let configNameStr = TH.nameBase configName
  objVar <- TH.newName "obj"

  -- Build the parser inside the lambda
  parserExp <- case fields of
    [] ->
      -- No fields: pure ConfigName
      return (TH.AppE (TH.VarE 'pure) (TH.ConE configName))
    (firstField : restFields) -> do
      -- Start with: ConfigName <$> obj .: "field1"
      firstParse <- fieldToParseExp objVar firstField
      let initial = TH.InfixE (Just (TH.ConE configName)) (TH.VarE 'fmap) (Just firstParse)
      -- Fold remaining: <*> obj .: "field2" <*> ...
      GhcMonad.foldM (addFieldParse objVar) initial restFields

  -- Build: withObject "ConfigName" $ \obj -> ...
  let lambda = TH.LamE [TH.VarP objVar] parserExp
  return
    ( TH.AppE
        (TH.AppE (TH.VarE 'Aeson.withObject) (TH.LitE (TH.StringL configNameStr)))
        lambda
      `TH.AppE` TH.VarE jsonVar
    )


-- | Generate parse expression for a field: obj .: "fieldName"
fieldToParseExp :: TH.Name -> FieldDef -> TH.Q TH.Exp
fieldToParseExp objVar fd = do
  let fieldNameStr = Text.unpack fd.fieldName
  let keyExp = TH.AppE (TH.VarE 'AesonKey.fromString) (TH.LitE (TH.StringL fieldNameStr))
  -- obj .: key
  return (TH.InfixE (Just (TH.VarE objVar)) (TH.VarE '(Aeson..:)) (Just keyExp))


-- | Add a field parse to the applicative chain
addFieldParse :: TH.Name -> TH.Exp -> FieldDef -> TH.Q TH.Exp
addFieldParse objVar acc fd = do
  parseExp <- fieldToParseExp objVar fd
  return (TH.InfixE (Just acc) (TH.VarE '(<*>)) (Just parseExp))


-- | Generate: instance ToJSON AppConfig where toJSON = ...
--
-- Generates a ToJSON instance that redacts secret fields.
-- Secret fields output "<REDACTED>" instead of the actual value.
generateToJsonInstance :: TH.Name -> [FieldDef] -> TH.Q TH.Dec
generateToJsonInstance configName fields = do
  -- Create parameter for the config value
  configVar <- TH.newName "config"

  -- Build the encoding body
  encodeBody <- buildToJsonBody configVar fields

  -- Create: toJSON config = object [...]
  let toJsonClause = TH.Clause [TH.VarP configVar] (TH.NormalB encodeBody) []

  return
    ( TH.InstanceD
        Nothing
        []
        (TH.ConT ''Aeson.ToJSON `TH.AppT` TH.ConT configName)
        [TH.FunD 'Aeson.toJSON [toJsonClause]]
    )


-- | Build the body of the toJSON function.
-- Generates: object ["field1" .= value1, "field2" .= "<REDACTED>", ...]
buildToJsonBody :: TH.Name -> [FieldDef] -> TH.Q TH.Exp
buildToJsonBody configVar fields = do
  -- Build the list of key-value pairs
  pairs <- GhcMonad.mapM (fieldToJsonPair configVar) fields
  let pairListExp = TH.ListE pairs
  -- object [...]
  return (TH.AppE (TH.VarE 'Aeson.object) pairListExp)


-- | Generate a JSON key-value pair for a field.
-- For secret fields: "fieldName" .= ("<REDACTED>" :: Text)
-- For normal fields: "fieldName" .= config.fieldName
fieldToJsonPair :: TH.Name -> FieldDef -> TH.Q TH.Exp
fieldToJsonPair configVar fd = do
  let fieldNameStr = Text.unpack fd.fieldName
  let isSecretField = hasSecretModifier fd.fieldModifiers
  let keyExp = TH.AppE (TH.VarE 'AesonKey.fromString) (TH.LitE (TH.StringL fieldNameStr))

  valueExp <- case isSecretField of
    True -> do
      -- Secret field: use "<REDACTED>" string
      return (TH.SigE (TH.LitE (TH.StringL "<REDACTED>")) (TH.ConT ''Text))
    False -> do
      -- Normal field: config.fieldName
      return (TH.GetFieldE (TH.VarE configVar) fieldNameStr)

  -- key .= value
  return (TH.InfixE (Just keyExp) (TH.VarE '(Aeson..=)) (Just valueExp))


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
--
-- For regular fields: @setting [env "VAR", help "...", ...]@
-- For nested fields: @subSettings "prefix"@ (uses the nested type's HasParser)
fieldToSettingExp :: FieldDef -> TH.Q TH.Exp
fieldToSettingExp fd = do
  let mods = fd.fieldModifiers
  let isNestedField = hasNestedModifier mods

  case isNestedField of
    True -> do
      -- Nested field: use subSettings with the prefix
      -- The prefix comes from envPrefix modifier, or defaults to the field name
      let prefix = case findEnvPrefix mods of
            Just p -> Text.unpack p
            Nothing -> Text.unpack fd.fieldName
      [|OptEnvConf.subSettings $(TH.litE (TH.stringL prefix))|]

    False -> do
      -- Regular field: setting [builder1, builder2, ..., reader auto/str]
      builders <- fieldModifiersToBuilders fd
      -- Use 'str' reader for Text/String types (IsString-based, accepts raw values)
      -- Use 'auto' reader for everything else (Read-based, expects Haskell syntax)
      readerExp <- case isTextLikeType fd.fieldType of
        True -> [|OptEnvConf.reader OptEnvConf.str|]
        False -> [|OptEnvConf.reader OptEnvConf.auto|]
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
    |> LinkedList.find isEnvVar
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
-- * @oauth2ClientId@ -> @OAUTH2_CLIENT_ID@
-- * @http2Enabled@ -> @HTTP2_ENABLED@
toEnvVarName :: Text -> Text
toEnvVarName name = do
  let chars = Text.unpack name
  let converted = insertUnderscores chars
  Text.toUpper (Text.pack converted)
 where
  insertUnderscores :: [Char] -> [Char]
  insertUnderscores [] = []
  insertUnderscores [c] = [c]
  insertUnderscores (c1 : c2 : rest) = do
    -- Insert underscore when:
    -- 1. lowercase -> UPPERCASE (e.g., "myName" -> "my_Name")
    -- 2. digit -> UPPERCASE (e.g., "oauth2Client" -> "oauth2_Client")
    let needsUnderscore =
          (GhcChar.isLower c1 && GhcChar.isUpper c2) ||
          (GhcChar.isDigit c1 && GhcChar.isUpper c2)
    case needsUnderscore of
      True ->
        c1 : '_' : insertUnderscores (c2 : rest)
      False ->
        c1 : insertUnderscores (c2 : rest)


-- | Generate type for implicit parameter: (?config :: AppConfig)
implicitParamType :: TH.Name -> TH.Type
implicitParamType configName =
  TH.ImplicitParamT "config" (TH.ConT configName)
