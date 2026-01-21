module Service.Query.TH (
  deriveQuery,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
import Data.Hashable qualified as Hashable
import Data.List qualified as GhcList
import GHC.Base (String)
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH


-- | Derive Query-related instances for a query type.
--
-- Usage:
--
-- @
-- -- Define auth functions in your query module
-- canAccess :: Maybe UserClaims -> Maybe QueryAuthError
-- canAccess = authenticatedAccess
--
-- canView :: Maybe UserClaims -> CartSummary -> Maybe QueryAuthError
-- canView = ownerOnly (.ownerId)
--
-- deriveQuery ''CartSummary [''CartEntity]
-- @
--
-- Generates:
--
-- * @type instance NameOf CartSummary = "CartSummary"@
-- * @type instance EntitiesOf CartSummary = '[CartEntity]@
-- * @instance Query CartSummary where canAccessImpl = canAccess; canViewImpl = canView@
-- * @instance KnownHash "CartSummary"@
--
-- Note: You must define 'canAccess' and 'canView' functions in your module.
-- You must also manually implement 'QueryOf' instances for each
-- entity-query relationship, as these contain business logic.
deriveQuery :: TH.Name -> [TH.Name] -> THLib.DecsQ
deriveQuery queryTypeName entityTypeNames = do
  let queryTypeStr = TH.nameBase queryTypeName

  -- Lookup required type families and classes
  nameOfTypeFamilyName <- lookupOrFail "NameOf"
  entitiesOfTypeFamilyName <- lookupOrFail "EntitiesOf"
  queryClassName <- lookupOrFail "Query"

  -- Generate: type instance NameOf QueryType = "QueryType"
  -- Uses the actual type name for dispatch matching (kebab-case is only for HTTP URLs)
  let nameOfInstance =
        TH.TySynInstD
          ( TH.TySynEqn
              Nothing
              (TH.ConT nameOfTypeFamilyName `TH.AppT` TH.ConT queryTypeName)
              (TH.LitT (TH.StrTyLit queryTypeStr))
          )

  -- Generate: type instance EntitiesOf QueryType = '[Entity1, Entity2]
  let entityTypeList =
        GhcList.foldr
          (\entityName acc -> TH.PromotedConsT `TH.AppT` TH.ConT entityName `TH.AppT` acc)
          TH.PromotedNilT
          entityTypeNames
  let entitiesOfInstance =
        TH.TySynInstD
          ( TH.TySynEqn
              Nothing
              (TH.ConT entitiesOfTypeFamilyName `TH.AppT` TH.ConT queryTypeName)
              entityTypeList
          )

  -- Generate: instance Query QueryType where
  --             canAccessImpl = canAccess
  --             canViewImpl = canView
  -- Use lookupValueName to ensure canAccess/canView are defined locally
  -- (not accidentally imported from elsewhere)
  canAccessName <-
    TH.lookupValueName "canAccess"
      >>= orError
        [fmt|
ERROR: Missing 'canAccess' function for query '#{queryTypeStr}'.

The 'canAccess' function defines who can access this query type.
It is checked BEFORE fetching any data from storage.

Please add the following function to your module:

  canAccess :: Maybe UserClaims -> Maybe QueryAuthError
  canAccess = authenticatedAccess  -- Secure default: requires login

Available helpers from Service.Query.Auth:
  - authenticatedAccess    -- Requires valid JWT token (RECOMMENDED DEFAULT)
  - requirePermission "x"  -- Requires specific permission
  - publicAccess           -- Anyone can access (use only for truly public data)
|]

  canViewName <-
    TH.lookupValueName "canView"
      >>= orError
        [fmt|
ERROR: Missing 'canView' function for query '#{queryTypeStr}'.

The 'canView' function defines who can view each instance of this query.
It is checked AFTER fetching data, allowing instance-level authorization.

Please add the following function to your module:

  canView :: Maybe UserClaims -> #{queryTypeStr} -> Maybe QueryAuthError
  canView = ownerOnly (.ownerId)  -- Secure default: only owner can view

Available helpers from Service.Query.Auth:
  - ownerOnly (.ownerId)   -- Only the owner can view (RECOMMENDED for user data)
  - publicView             -- Anyone can view (use only after canAccess check)
|]

  let queryInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT queryClassName `TH.AppT` TH.ConT queryTypeName)
          [ TH.FunD
              (TH.mkName "canAccessImpl")
              [TH.Clause [] (TH.NormalB (TH.VarE canAccessName)) []],
            TH.FunD
              (TH.mkName "canViewImpl")
              [TH.Clause [] (TH.NormalB (TH.VarE canViewName)) []]
          ]

  -- Generate KnownHash instance
  knownHashInstances <- deriveKnownHash queryTypeStr

  pure ([nameOfInstance, entitiesOfInstance, queryInstance] ++ knownHashInstances)


-- | Derives a KnownHash instance for a given string at compile time.
-- This generates: instance KnownHash "yourString" where hashVal _ = <computed hash>
deriveKnownHash :: String -> THLib.DecsQ
deriveKnownHash nameStr = do
  knownHashClassName <-
    TH.lookupTypeName "KnownHash"
      >>= orError
        [fmt|
ERROR: KnownHash type class not found.

Please ensure you have `import Core` at the top of your module.
|]

  let nameLit = TH.LitT (TH.StrTyLit nameStr)

  -- Compute hash at compile time using hashable
  let nameHash = Hashable.hash nameStr

  -- Generate: instance KnownHash "nameStr" where
  --             hashVal _ = <computed hash>
  let knownHashInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT knownHashClassName `TH.AppT` nameLit)
          [ TH.FunD
              (TH.mkName "hashVal")
              [ TH.Clause
                  [TH.WildP]
                  (TH.NormalB (TH.LitE (TH.IntegerL (fromIntegral nameHash))))
                  []
              ]
          ]

  pure [knownHashInstance]


lookupOrFail :: String -> TH.Q TH.Name
lookupOrFail name = do
  result <- TH.lookupTypeName name
  case result of
    Just n -> pure n
    Nothing ->
      MonadFail.fail
        [fmt|Could not find type: #{name}. Ensure you have `import Core` at the top of your module.|]


orError :: String -> Maybe a -> TH.Q a
orError errMsg mVal =
  case mVal of
    Just x -> pure x
    Nothing -> MonadFail.fail errMsg
