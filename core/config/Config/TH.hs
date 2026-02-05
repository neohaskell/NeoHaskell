-- | Template Haskell macro for the NeoHaskell Config DSL.
--
-- This module provides the 'defineConfig' macro that generates:
--
--   1. A record data type from field specifications
--   2. A 'Default' instance
--   3. A parser that validates all fields at startup
--   4. A @HasXxxConfig@ type alias for convenient constraints
--
-- = Usage
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
--
-- import Config.TH (defineConfig)
-- import Config.Builder qualified as Config
--
-- defineConfig "AppConfig"
--   [ Config.field \@Int "port"
--       |> Config.doc "HTTP port"
--       |> Config.defaultsTo 8080
--   , Config.field \@Text "databaseUrl"
--       |> Config.doc "Database connection string"
--       |> Config.required
--   ]
-- @
--
-- = Generated Code
--
-- The macro generates:
--
-- @
-- data AppConfig = AppConfig
--   { port :: Int
--   , databaseUrl :: Text
--   }
--   deriving (Show, Eq, Generic)
--
-- instance Default AppConfig where
--   def = AppConfig
--     { port = 8080
--     , databaseUrl = error "Required field: databaseUrl"
--     }
--
-- type HasAppConfig = HasConfig AppConfig
--
-- loadAppConfig :: IO AppConfig
-- loadAppConfig = ...
-- @
module Config.TH (
  -- * Main Macro
  defineConfig,

  -- * Helper Types (for advanced use)
  ConfigField (..),
  mkConfigField,
) where

import Array qualified
import Config.Core (FieldSource (..), FieldSpec (..), Optionality (..))
import Config.HasConfig (HasConfig)
-- Config.Naming imported but not yet used (available for future extensions)
import Config.Parser qualified as Parser
import Control.Monad.Fail qualified as MonadFail
import Core
import Data.Foldable qualified as Foldable
import Data.List qualified as GhcList
import Data.Maybe qualified as GhcMaybe
import GHC.Base (String)
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH
import Text qualified
import Prelude qualified
import Prelude ((.),(<$>))


-- | A simplified field description for TH code generation.
--
-- Since 'FieldSpec' is parameterized by value type, we need this
-- untyped representation to collect multiple fields in a list.
data ConfigField = ConfigField
  { cfName :: String
  , -- ^ Field name (camelCase)
    cfType :: TH.Name
  , -- ^ Type name (e.g., ''Int, ''Text)
    cfDoc :: Maybe String
  , -- ^ Documentation string
    cfDefault :: Maybe TH.Exp
  , -- ^ Default value expression (Nothing = required)
    cfEnvVar :: String
  , -- ^ Environment variable name
    cfIsSecret :: Bool
    -- ^ Whether to redact this field in logs and error messages
  }
  deriving (Show)


-- | Create a config field specification for use with 'defineConfig'.
--
-- This is a simplified builder that captures field information for
-- Template Haskell code generation.
--
-- ==== __Examples__
--
-- @
-- -- A field with a default value
-- mkConfigField ''Int "port" (Just "HTTP port") (Just [| 8080 |]) "PORT" False
--
-- -- A required secret field
-- mkConfigField ''Text "apiKey" (Just "API Key") Nothing "API_KEY" True
-- @
mkConfigField ::
  TH.Name ->
  -- ^ Type name (e.g., ''Int)
  String ->
  -- ^ Field name
  Maybe String ->
  -- ^ Documentation
  Maybe (TH.Q TH.Exp) ->
  -- ^ Default value (Nothing = required)
  String ->
  -- ^ Environment variable name
  Bool ->
  -- ^ Whether this field is a secret (default: False)
  TH.Q ConfigField
mkConfigField typeName fieldName docStr maybeDefault envVarName isSecret = do
  defaultExp <- case maybeDefault of
    Just expQ -> Just <$> expQ
    Nothing -> pure Nothing
  pure
    ConfigField
      { cfName = fieldName
      , cfType = typeName
      , cfDoc = docStr
      , cfDefault = defaultExp
      , cfEnvVar = envVarName
      , cfIsSecret = isSecret
      }


-- | Generate a complete configuration type with all supporting code.
--
-- This macro takes a type name and a list of field specifications,
-- and generates:
--
--   1. A record data type with the specified fields
--   2. Deriving clauses for Show, Eq, and Generic
--   3. A Default instance using the specified defaults
--   4. A HasXxxConfig type alias
--   5. A loadXxxConfig function for parsing from environment
--
-- ==== __Example__
--
-- @
-- defineConfig "ServerConfig"
--   [ mkConfigField ''Int "port" (Just "HTTP port") (Just [| 8080 |]) "PORT"
--   , mkConfigField ''Text "host" (Just "Host to bind") (Just [| "localhost" |]) "HOST"
--   , mkConfigField ''Text "databaseUrl" (Just "Database URL") Nothing "DATABASE_URL"
--   ]
-- @
--
-- This generates:
--
-- @
-- data ServerConfig = ServerConfig
--   { port :: Int
--   , host :: Text
--   , databaseUrl :: Text
--   }
--   deriving (Show, Eq, Generic)
--
-- instance Default ServerConfig where
--   def = ServerConfig
--     { port = 8080
--     , host = "localhost"
--     , databaseUrl = error "Required field 'databaseUrl': set DATABASE_URL"
--     }
--
-- type HasServerConfig = HasConfig ServerConfig
--
-- loadServerConfig :: IO ServerConfig
-- loadServerConfig = do
--   port <- parseOrFail "port" "PORT" (Just 8080)
--   host <- parseOrFail "host" "HOST" (Just "localhost")
--   databaseUrl <- parseOrFail "databaseUrl" "DATABASE_URL" Nothing
--   pure ServerConfig{port, host, databaseUrl}
-- @
defineConfig :: String -> [TH.Q ConfigField] -> THLib.DecsQ
defineConfig configNameStr fieldsQ = do
  -- Collect all field specifications
  fields <- Prelude.sequence fieldsQ

  -- Validate we have at least one field
  when (GhcList.null fields)
    (MonadFail.fail "defineConfig: At least one field is required")

  -- Validate field specifications
  validateFields configNameStr fields

  -- Generate declarations
  let configName = TH.mkName configNameStr
  let configConName = TH.mkName configNameStr

  -- 1. Generate the data type declaration
  dataDecl <- generateDataType configName configConName fields

  -- 2. Generate Default instance
  defaultInstance <- generateDefaultInstance configName configConName fields

  -- 3. Generate HasXxxConfig type alias
  hasConfigAlias <- generateHasConfigAlias configNameStr configName

  -- 4. Generate loadXxxConfig function
  loadFunction <- generateLoadFunction configNameStr configName configConName fields

  pure (dataDecl ++ [defaultInstance] ++ hasConfigAlias ++ loadFunction)


-- | Validate all field specifications at compile time.
--
-- Enforces:
--   1. Every field must have documentation (non-empty)
--   2. Every field must have exactly one of: defaultsTo OR required
validateFields :: String -> [ConfigField] -> TH.Q ()
validateFields configNameStr fields =
  Foldable.traverse_ (validateField configNameStr) fields


-- | Validate a single field specification.
validateField :: String -> ConfigField -> TH.Q ()
validateField configNameStr field = do
  -- Check for missing documentation
  case field.cfDoc of
    Nothing ->
      MonadFail.fail
        ( "defineConfig \""
            ++ configNameStr
            ++ "\": Field '"
            ++ field.cfName
            ++ "' is missing documentation.\n\n"
            ++ "  Add:\n"
            ++ "    |> Config.doc \"Description of this field\""
        )
    Just doc ->
      when (GhcList.null doc)
        ( MonadFail.fail
            ( "defineConfig \""
                ++ configNameStr
                ++ "\": Field '"
                ++ field.cfName
                ++ "' has empty documentation.\n\n"
                ++ "  Provide a meaningful description:\n"
                ++ "    |> Config.doc \"Description of this field\""
            )
        )


-- | Generate the record data type declaration.
--
-- Produces:
--
-- @
-- data ConfigName = ConfigName
--   { field1 :: Type1
--   , field2 :: Type2
--   }
--   deriving (Show, Eq, Generic)
-- @
generateDataType :: TH.Name -> TH.Name -> [ConfigField] -> THLib.DecsQ
generateDataType typeName conName fields = do
  -- Build record fields
  let recordFields =
        fields
          |> GhcList.map
            ( \field ->
                ( TH.mkName field.cfName
                , TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
                , TH.ConT field.cfType
                )
            )

  -- Create the record constructor
  let recordCon = TH.RecC conName recordFields

  -- Derive clauses
  let deriveClause =
        TH.DerivClause
          Nothing
          [ TH.ConT ''Show
          , TH.ConT ''Eq
          , TH.ConT ''Generic
          ]

  -- Build the data declaration
  let dataDecl =
        TH.DataD
          [] -- No context
          typeName
          [] -- No type variables
          Nothing -- No kind signature
          [recordCon]
          [deriveClause]

  pure [dataDecl]


-- | Generate the Default instance.
--
-- For fields with defaults, uses the default value.
-- For required fields, uses 'error' with a helpful message.
generateDefaultInstance :: TH.Name -> TH.Name -> [ConfigField] -> THLib.DecQ
generateDefaultInstance typeName conName fields = do
  -- Look up the Default class
  defaultClassName <- lookupOrFail "Default"

  -- Build field expressions for the record
  fieldExps <-
    fields
      |> Prelude.traverse
        ( \field -> do
            let fieldName = TH.mkName field.cfName
            valueExp <- case field.cfDefault of
              Just defaultVal ->
                pure defaultVal
              Nothing -> do
                -- Required field - use error with helpful message
                let errMsg =
                      "Required field '"
                        ++ field.cfName
                        ++ "': set "
                        ++ field.cfEnvVar
                        ++ " environment variable"
                pure (TH.AppE (TH.VarE 'Prelude.error) (TH.LitE (TH.StringL errMsg)))
            pure (fieldName, valueExp)
        )

  -- Build the record expression
  let recordExp = TH.RecConE conName fieldExps

  -- Build the instance
  let instanceType = TH.ConT defaultClassName `TH.AppT` TH.ConT typeName
  let defDecl =
        TH.FunD
          (TH.mkName "def")
          [TH.Clause [] (TH.NormalB recordExp) []]

  pure (TH.InstanceD Nothing [] instanceType [defDecl])


-- | Generate the HasXxxConfig type alias.
--
-- Produces:
--
-- @
-- type HasAppConfig = HasConfig AppConfig
-- @
generateHasConfigAlias :: String -> TH.Name -> THLib.DecsQ
generateHasConfigAlias configNameStr configTypeName = do
  let aliasName = TH.mkName ("Has" ++ configNameStr)
  let hasConfigType = TH.ConT ''HasConfig `TH.AppT` TH.ConT configTypeName

  pure [TH.TySynD aliasName [] hasConfigType]


-- | Generate the loadXxxConfig function.
--
-- This function parses all configuration fields from environment variables
-- and returns either the complete config or exits with formatted errors.
--
-- Produces:
--
-- @
-- loadAppConfig :: IO AppConfig
-- loadAppConfig = do
--   portResult <- parseEnvVar "port" "PORT" (Just 8080)
--   dbResult <- parseEnvVar "databaseUrl" "DATABASE_URL" Nothing
--   case (portResult, dbResult) of
--     (Ok port, Ok db) -> pure AppConfig{port, databaseUrl = db}
--     _ -> do
--       let errors = collectErrors [("port", portResult), ...]
--       formatAndExit errors
-- @
generateLoadFunction :: String -> TH.Name -> TH.Name -> [ConfigField] -> THLib.DecsQ
generateLoadFunction configNameStr configTypeName configConName fields = do
  let loadFnName = TH.mkName ("load" ++ configNameStr)

  -- Type signature: loadXxxConfig :: IO ConfigType
  let typeSig = TH.SigD loadFnName (TH.AppT (TH.ConT ''IO) (TH.ConT configTypeName))

  -- Generate variable names for each field's result
  let fieldResultNames = fields |> GhcList.map (\f -> TH.mkName (f.cfName ++ "Result"))

  -- Build the function body
  bodyExp <- generateLoadFunctionBody configConName fields fieldResultNames

  let funDecl =
        TH.FunD
          loadFnName
          [TH.Clause [] (TH.NormalB bodyExp) []]

  pure [typeSig, funDecl]


-- | Generate the body of the load function.
generateLoadFunctionBody :: TH.Name -> [ConfigField] -> [TH.Name] -> THLib.ExpQ
generateLoadFunctionBody configConName fields resultNames = do
  -- We'll generate a simple version that uses Parser.parseField
  -- and collects errors

  -- For each field, generate a binding:
  --   fieldResult <- parseFieldFromEnv "FIELD_NAME" (Just defaultValue)
  let parseBindings =
        GhcList.zipWith
          ( \field resultName ->
              TH.BindS
                (TH.VarP resultName)
                (generateParseCall field)
          )
          fields
          resultNames

  -- Generate the success case where all fields are Ok
  let fieldNames = fields |> GhcList.map (\f -> TH.mkName f.cfName)

  -- Build pattern for all Ok results
  let successPattern =
        TH.TupP
          ( resultNames
              |> GhcList.zipWith
                ( \_resultName fieldName ->
                    TH.ConP 'Ok [] [TH.VarP fieldName]
                )
                fieldNames
          )

  -- Build the success expression: pure ConfigName{field1, field2, ...}
  let recordBinds = fieldNames |> GhcList.map (\n -> (n, TH.VarE n))
  let successExp =
        TH.AppE
          (TH.VarE 'pure)
          (TH.RecConE configConName recordBinds)

  -- Build the error case
  let errorPattern = TH.WildP
  let errorExp = generateErrorHandling fields resultNames

  -- Build the case expression
  let resultTuple = TH.TupE (resultNames |> GhcList.map (Just . TH.VarE))
  let caseExp =
        TH.CaseE
          resultTuple
          [ TH.Match successPattern (TH.NormalB successExp) []
          , TH.Match errorPattern (TH.NormalB errorExp) []
          ]

  -- Wrap in do-notation
  let doExp = TH.DoE Nothing (parseBindings ++ [TH.NoBindS caseExp])

  pure doExp


-- | Generate the parse call for a single field.
generateParseCall :: ConfigField -> TH.Exp
generateParseCall field =
  let envVarLit = TH.LitE (TH.StringL field.cfEnvVar)
      fieldNameLit = TH.LitE (TH.StringL field.cfName)
      isSecretLit = TH.ConE (if field.cfIsSecret then 'True else 'False)
      -- Use a helper function that we'll define
      -- parseEnvField :: Read a => String -> String -> Maybe a -> Bool -> IO (Result ConfigError a)
   in TH.AppE
        ( TH.AppE
            ( TH.AppE
                ( TH.AppE
                    (TH.VarE 'parseEnvField)
                    fieldNameLit
                )
                envVarLit
            )
            ( case field.cfDefault of
                Just defExp -> TH.AppE (TH.ConE 'Just) defExp
                Nothing -> TH.ConE 'Nothing
            )
        )
        isSecretLit


-- | Generate error handling code that collects and reports all errors.
generateErrorHandling :: [ConfigField] -> [TH.Name] -> TH.Exp
generateErrorHandling fields resultNames =
  -- Generate: do
  --   let errors = collectErrors [(name, result), ...]
  --   Parser.runParser (pure (Err errors))
  let pairs =
        GhcList.zipWith
          ( \field resultName ->
              TH.TupE
                [ Just (TH.LitE (TH.StringL field.cfName))
                , Just (TH.VarE resultName)
                ]
          )
          fields
          resultNames
      errorListExp = TH.ListE pairs
      collectExp = TH.AppE (TH.VarE 'collectConfigErrors) errorListExp
      errorsVar = TH.VarE (TH.mkName "errors")
      runParserExp =
        TH.AppE
          (TH.VarE 'Parser.runParser)
          ( TH.AppE
              (TH.VarE 'pure)
              (TH.AppE (TH.ConE 'Err) errorsVar)
          )
   in TH.DoE
        Nothing
        [ TH.LetS [TH.ValD (TH.VarP (TH.mkName "errors")) (TH.NormalB collectExp) []]
        , TH.NoBindS runParserExp
        ]


-- | Parse a single field from an environment variable.
--
-- This helper is used by generated load functions.
parseEnvField ::
  forall value.
  (Prelude.Read value) =>
  String ->
  -- ^ Field name (for error messages)
  String ->
  -- ^ Environment variable name
  Maybe value ->
  -- ^ Default value (Nothing = required)
  Bool ->
  -- ^ Whether this field is a secret
  IO (Result Parser.ConfigError value)
parseEnvField fieldName envVarName maybeDefault isSecret = do
  let fieldNameText = Text.fromLinkedList fieldName
  let envVarText = Text.fromLinkedList envVarName

  -- Create a minimal FieldSpec for parsing
  let spec :: FieldSpec value
      spec =
        FieldSpec
          { fieldName = fieldNameText
          , fieldType = "value" -- Type name placeholder (actual type is known at TH generation time)
          , fieldDoc = Nothing
          , fieldOptionality = case maybeDefault of
              Just v -> HasDefault v
              Nothing -> Required
          , fieldSources = Array.wrap (EnvVarSource envVarText)
          , fieldIsSecret = isSecret
          }

  Parser.parseField spec


-- | Collect errors from a list of parse results.
--
-- Filters out successful results and collects only errors.
collectConfigErrors ::
  [(String, Result Parser.ConfigError value)] ->
  Array Parser.ConfigError
collectConfigErrors pairs =
  pairs
    |> GhcMaybe.mapMaybe
      ( \(_, result) ->
          case result of
            Err err -> Just err
            Ok _ -> Nothing
      )
    |> Array.fromLinkedList


-- | Look up a type name or fail with a helpful error.
lookupOrFail :: String -> TH.Q TH.Name
lookupOrFail name = do
  result <- TH.lookupTypeName name
  case result of
    Just n -> pure n
    Nothing ->
      MonadFail.fail
        ("Could not find type: " ++ name ++ ". Ensure you have `import Core` at the top of your module.")


-- | Helper for when condition (re-exported for internal use)
when :: Bool -> TH.Q () -> TH.Q ()
when True action = action
when False _ = pure ()
