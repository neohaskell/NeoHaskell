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
import Text qualified


-- | Derive Query-related instances for a query type.
--
-- Usage:
--
-- @
-- deriveQuery ''UserOrders ['UserEntity, 'OrderEntity]
-- @
--
-- Generates:
--
-- * @type instance NameOf UserOrders = "user-orders"@
-- * @type instance EntitiesOf UserOrders = '[UserEntity, OrderEntity]@
-- * @instance Query UserOrders@
-- * @instance KnownHash "UserOrders"@
--
-- Note: You must still manually implement 'QueryOf' instances for each
-- entity-query relationship, as these contain business logic.
deriveQuery :: TH.Name -> [TH.Name] -> THLib.DecsQ
deriveQuery queryTypeName entityTypeNames = do
  let queryTypeStr = TH.nameBase queryTypeName

  -- Convert to kebab-case for URL: UserOrders -> user-orders
  let kebabName = Text.toKebabCase (Text.fromLinkedList queryTypeStr) |> Text.toLinkedList

  -- Lookup required type families and classes
  nameOfTypeFamilyName <- lookupOrFail "NameOf"
  entitiesOfTypeFamilyName <- lookupOrFail "EntitiesOf"
  queryClassName <- lookupOrFail "Query"

  -- Generate: type instance NameOf QueryType = "query-type"
  let nameOfInstance =
        TH.TySynInstD
          ( TH.TySynEqn
              Nothing
              (TH.ConT nameOfTypeFamilyName `TH.AppT` TH.ConT queryTypeName)
              (TH.LitT (TH.StrTyLit kebabName))
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

  -- Generate: instance Query QueryType
  let queryInstance =
        TH.InstanceD
          Nothing
          []
          (TH.ConT queryClassName `TH.AppT` TH.ConT queryTypeName)
          []

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
