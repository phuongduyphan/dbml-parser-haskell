{-# LANGUAGE OverloadedStrings #-}

module DBML
  ( DBML.parse
  , normalize
  , DBMLState(..)
  )
where

import           Text.Megaparsec               as MP
import           DBML.Parser
import           DBML.Lexer
import           DBML.Utils
import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )
import           Data.List                      ( find )
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad                  ( liftM )
import           Debug.Trace                    ( trace )

type Id = Int

data DBMLState = DBMLState
  { tableS :: Map.Map Id NTable
  , enumS :: Map.Map Id NEnum
  , refS :: Map.Map Id NRef
  , tableGroupS :: Map.Map Id NTableGroup
  , fieldS :: Map.Map Id NField
  , indexS :: Map.Map Id NIndex
  , endpointS :: Map.Map Id NEndpoint
  , tableIdCounter :: Id
  , enumIdCounter :: Id
  , refIdCounter :: Id
  , tableGroupIdCounter :: Id
  , fieldIdCounter :: Id
  , indexIdCounter :: Id
  , endpointIdCounter :: Id
  } deriving (Show)

data NTable = NTable
  { ntId :: Id
  , ntGroupId :: Maybe Id
  , ntName :: Text
  , ntTableSettings :: Maybe [TableSetting]
  , ntFieldIds :: [Id]
  , ntIndexIds :: [Id]
  } deriving (Show)

data NEnum = NEnum
  { neId :: Id
  , neName :: Text
  , neValues :: [EnumValue]
  , neFieldIds :: [Id]
  } deriving (Show)

data NRef = NRef
  { nrId :: Id
  , nrName :: Maybe Text
  , nrValueRelation :: RefRelation
  , nrValueSettings :: Maybe [RefSetting]
  , nrEndpointIds :: [Id]
  } deriving (Show)

data NEndpoint = NEndpoint
  { nepId :: Id
  , nepTableName :: Text
  , nepFieldName :: Text
  , nepRefId :: Id
  , nepFieldId :: Id
  } deriving (Show)

data NTableGroup = NTableGroup
  { ntgId :: Id
  , ntgName :: Text
  , ntgTableIds :: [Id]
  } deriving (Show)

data NField = NField
  { nfId :: Id
  , nfTableId :: Id
  , nfEnumId :: Maybe Id
  , nfName :: Text
  , nfType :: Text
  , nfFieldSettings :: Maybe [FieldSetting]
  } deriving (Show)

data NIndex = NIndex
  { niId :: Id
  , niTableId :: Id
  , niIndexSettings :: Maybe [IndexSetting]
  , niIndexIdentifiers :: [IndexIdentifier]
  } deriving (Show)

type DBMLMonad = StateT DBMLState (Either Text)

parse :: Text -> Either (ParseErrorBundle Text Void) Database
parse = MP.parse pDatabase ""

normalize :: Database -> DBMLMonad ()
normalize db = do
  buildTableMap db
  buildEnumMap db
  buildTableGroupMap db
  buildRefMap db
  return ()

buildTableMap :: Database -> DBMLMonad ()
buildTableMap (Database xs) = mapM_ buildTable tables
 where
  tables = filter isTable xs
  isTable (DBMLTable _) = True
  isTable _             = False
  buildTable (DBMLTable table) = do
    state <- get
    let tableId = tableIdCounter state
    put state { tableIdCounter = tableId + 1 }
    fieldMap <- getFieldMap table tableId
    validateFieldsInTable table (map snd (Map.toList fieldMap))
    state1 <- get
    put state1 { fieldS = Map.union (fieldS state1) fieldMap }
    indexMap <- getIndexMap table tableId
    validateIndexesInTable table (map snd (Map.toList indexMap))
    let tableMap = Map.fromList
          [ ( tableId
            , NTable { ntId            = tableIdCounter state
                     , ntGroupId       = Nothing
                     , ntName          = tableName table
                     , ntTableSettings = tableSettings table
                     , ntFieldIds      = map fst (Map.toList fieldMap)
                     , ntIndexIds      = map fst (Map.toList indexMap)
                     }
            )
          ]
    state2 <- get
    put state2 { indexS = Map.union (indexS state2) indexMap
               , tableS = Map.union (tableS state) tableMap
               }
    return ()

validateFieldsInTable :: Table -> [NField] -> DBMLMonad ()
validateFieldsInTable table fields = case allDifferent (map nfName fields) of
  Left fname -> lift
    (          Left
    $          "Field "
    `T.append` fname
    `T.append` " existed in table "
    `T.append` tableName table
    )
  _ -> lift (Right ())

validateIndexesInTable :: Table -> [NIndex] -> DBMLMonad ()
validateIndexesInTable table indexes =
  case
      subList (map getIndexColumnName indexColumns) (map getFieldName fields)
    of
      Left iName -> lift
        (          Left
        $          "Index Column "
        `T.append` iName
        `T.append` " do not exist in table "
        `T.append` tableName table
        )
      _ -> lift (Right ())
 where
  fields = filter isField (fromMaybe [] (tableValues table))
  isField (TableField _) = True
  isField _              = False
  indexColumns = concatMap indexColumn indexes
  indexColumn index = filter isIndexColumn (niIndexIdentifiers index)
  isIndexColumn (IndexColumn _) = True
  isIndexColumn _               = False
  getIndexColumnName (IndexColumn c) = c
  getFieldName (TableField f) = fieldName f


getFieldMap :: Table -> Id -> DBMLMonad (Map.Map Id NField)
getFieldMap table id = Map.fromList <$> mapM buildField fields
 where
  fields = filter isField (fromMaybe [] (tableValues table))
  isField (TableField _) = True
  isField _              = False
  buildField (TableField field) = do
    state <- get
    let fieldId = fieldIdCounter state
    put state { fieldIdCounter = fieldId + 1 }
    return
      ( fieldId
      , NField { nfId            = fieldId
               , nfTableId       = id
               , nfEnumId        = Nothing
               , nfName          = fieldName field
               , nfType          = fieldType field
               , nfFieldSettings = fieldSettings field
               }
      )

getIndexMap :: Table -> Id -> DBMLMonad (Map.Map Id NIndex)
getIndexMap table id = Map.fromList <$> mapM buildIndex indexes
 where
  indexes = concatMap (\(TableIndexes xs) -> xs)
                      (filter isIndex (fromMaybe [] (tableValues table)))
  isIndex (TableIndexes _) = True
  isIndex _                = False
  buildIndex index = do
    state <- get
    let indexId = indexIdCounter state
    put state { indexIdCounter = indexId + 1 }
    return
      ( indexId
      , NIndex { niId               = indexId
               , niTableId          = id
               , niIndexIdentifiers = indexIdentifiers index
               , niIndexSettings    = indexSettings index
               }
      )

buildEnumMap :: Database -> DBMLMonad ()
buildEnumMap (Database xs) = mapM_ buildEnum enums
 where
  enums = filter isEnum xs
  isEnum (DBMLEnum _) = True
  isEnum _            = False
  buildEnum (DBMLEnum enum) = do
    state <- get
    let enumId = enumIdCounter state
    put state { enumIdCounter = enumId + 1 }
    validateEnumValueInEnum enum
    fieldIds <- getEnumFieldIds enum enumId
    let enumMap = Map.fromList
          [ ( enumId
            , NEnum { neId       = enumId
                    , neName     = enumName enum
                    , neValues   = enumValues enum
                    , neFieldIds = fieldIds
                    }
            )
          ]
    state1 <- get
    put state1 { enumS = Map.union (enumS state1) enumMap }
    return ()


validateEnumValueInEnum :: DBML.Parser.Enum -> DBMLMonad ()
validateEnumValueInEnum enum =
  case allDifferent (map enumValue (enumValues enum)) of
    Left val -> lift
      (Left
        ("Enum value " `T.append` "existed in enum " `T.append` enumName enum)
      )
    _ -> lift (Right ())

getEnumFieldIds :: DBML.Parser.Enum -> Id -> DBMLMonad [Id]
getEnumFieldIds enum id = do
  state <- get
  let fields = filter (\nf -> nfType nf == enumName enum)
                      (map snd (Map.toList (fieldS state)))
  mapM_ (updateFieldEnumId id) fields
  return (map nfId fields)

updateFieldEnumId :: Id -> NField -> DBMLMonad ()
updateFieldEnumId enumId field = do
  state <- get
  let fieldMaybe = Map.lookup (nfId field) (fieldS state)
  case fieldMaybe of
    Nothing -> lift
      (Left
        ("Cannot find field with id " `T.append` (T.pack . show) (nfId field))
      )
    Just field' -> do
      put state
        { fieldS = Map.insert (nfId field')
                              (field' { nfEnumId = Just enumId })
                              (fieldS state)
        }
      return ()

buildTableGroupMap :: Database -> DBMLMonad ()
buildTableGroupMap (Database xs) = mapM_ buildTableGroup tableGroups
 where
  tableGroups = filter isTableGroup xs
  isTableGroup (DBMLTableGroup _) = True
  isTableGroup _                  = False
  buildTableGroup (DBMLTableGroup tg) = do
    state <- get
    let tgId = tableGroupIdCounter state
    put state { tableGroupIdCounter = tgId + 1 }
    validateTableInTg tg (map snd (Map.toList (tableS state)))
    tableIds <- getTgTableIds tg tgId
    let tgMap = Map.fromList
          [ ( tgId
            , NTableGroup { ntgId       = tgId
                          , ntgName     = tableGroupName tg
                          , ntgTableIds = tableIds
                          }
            )
          ]
    state1 <- get
    put state1 { tableGroupS = Map.union (tableGroupS state1) tgMap }
    return ()

validateTableInTg :: TableGroup -> [NTable] -> DBMLMonad ()
validateTableInTg tg tables = do
  case subList (map tgTableName (tableGroupValues tg)) (map ntName tables) of
    Left tName ->
      lift (Left ("Table " `T.append` tName `T.append` " don't exist"))
    _ -> return ()
  case allDifferent (map tgTableName (tableGroupValues tg)) of
    Left tName' ->
      lift
        (Left ("Table " `T.append` tName' `T.append` " is already in the group")
        )
    _ -> return ()
  state <- get
  case
      allSatisfy (isNothing . fst) (map (\t -> (ntGroupId t, ntName t)) tables)
    of
      Left (Just groupId, tableName) -> lift
        (Left
          (          "Table "
          `T.append` tableName
          `T.append` " is already in group "
          `T.append` maybe "" ntgName (Map.lookup groupId (tableGroupS state))
          )
        )
      _ -> return ()

getTgTableIds :: TableGroup -> Id -> DBMLMonad [Id]
getTgTableIds tg id = do
  state <- get
  let tables = filter
        (\nt -> ntName nt `elem` map tgTableName (tableGroupValues tg))
        (map snd (Map.toList (tableS state)))
  mapM_ (updateTableTgId id) tables
  return (map ntId tables)

updateTableTgId :: Id -> NTable -> DBMLMonad ()
updateTableTgId tgId table = do
  state <- get
  let tableMaybe = Map.lookup (ntId table) (tableS state)
  case tableMaybe of
    Nothing -> lift
      (Left
        ("Cannot find table with id " `T.append` (T.pack . show) (ntId table))
      )
    Just table' -> do
      put state
        { tableS = Map.insert (ntId table')
                              (table' { ntGroupId = Just tgId })
                              (tableS state)
        }
      return ()

buildRefMap :: Database -> DBMLMonad ()
buildRefMap (Database xs) = mapM_ buildRef (refs ++ inlineRefs)
 where
  refs = map (\(DBMLRef r) -> r) (filter isRef xs)
  isRef (DBMLRef _) = True
  isRef _           = False
  inlineRefs = getInlineRefs (Database xs)
  buildRef ref = do
    state <- get
    let refId = refIdCounter state
    put state { refIdCounter = refId + 1 }
    endpointMap <- getEndPointMap ref refId
    let refMap = Map.fromList
          [ ( refId
            , NRef { nrId            = refId
                   , nrName          = refName ref
                   , nrValueRelation = refValueRelation (refValue ref)
                   , nrValueSettings = refValueSettings (refValue ref)
                   , nrEndpointIds   = map fst (Map.toList endpointMap)
                   }
            )
          ]
    state1 <- get
    put state1 { refS      = Map.union (refS state1) refMap
               , endpointS = Map.union (endpointS state1) endpointMap
               }
    return ()

getInlineRefs :: Database -> [Ref]
getInlineRefs (Database xs) = concatMap getInlineRefsFromTable tables
 where
  tables = map (\(DBMLTable t) -> t) (filter isTable xs)
  isTable (DBMLTable _) = True
  isTable _             = False
  getInlineRefsFromTable table = case tableValues table of
    Nothing       -> []
    Just tbValues -> concatMap (getInlineRefsFromField . (\(TableField f) -> f)) (filter isTableField tbValues)
   where
    getInlineRefsFromField field = case fieldSettings field of
      Nothing        -> []
      Just fSettings -> case find isFieldRefInline fSettings of
        Nothing -> []
        Just (FieldRefInline inlineRef) ->
          [ (Ref
              { refName  = Nothing
              , refValue =
                RefValue
                  { refValueEndpoints =
                    [ RefEndpoint { endpointTableName = tableName table
                                  , endpointFieldName = fieldName field
                                  }
                    , RefEndpoint
                      { endpointTableName = refInlineTableName inlineRef
                      , endpointFieldName = refInlineFieldName inlineRef
                      }
                    ]
                  , refValueRelation  = refInlineRelation inlineRef
                  , refValueSettings  = Nothing
                  }
              }
            )
          ]
    isFieldRefInline (FieldRefInline _) = True
    isFieldRefInline _                  = False
  isTableField (TableField _) = True
  isTableField _              = False

getEndPointMap :: Ref -> Id -> DBMLMonad (Map.Map Id NEndpoint)
getEndPointMap ref id = Map.fromList <$> mapM buildEndpoint endpoints
 where
  endpoints = refValueEndpoints (refValue ref)
  buildEndpoint endpoint = do
    state <- get
    let endpointId = endpointIdCounter state
    put state { endpointIdCounter = endpointId + 1 }
    field <- findEndpointField endpoint
    return
      ( endpointId
      , NEndpoint { nepId        = endpointId
                  , nepTableName = endpointTableName endpoint
                  , nepFieldName = endpointFieldName endpoint
                  , nepRefId     = id
                  , nepFieldId   = nfId field
                  }
      )

findEndpointField :: RefEndpoint -> DBMLMonad NField
findEndpointField endpoint = do
  state <- get
  let tableMaybe = find
        (\table -> ntName table == endpointTableName endpoint)
        (map snd (Map.toList (tableS state)))
  case tableMaybe of
    Nothing -> lift
      (Left
        (          "Cannot find table "
        `T.append` (T.pack . show) (endpointTableName endpoint)
        `T.append` " in schema"
        )
      )
    Just table -> do
      fields <- mapM findFieldById (ntFieldIds table)
      let fieldMaybe =
            find (\field -> nfName field == endpointFieldName endpoint) fields
      case fieldMaybe of
        Nothing -> lift
          (Left
            (          "Cannot find field "
            `T.append` (T.pack . show) (endpointFieldName endpoint)
            `T.append` " in table "
            `T.append` (T.pack . show) (ntName table)
            )
          )
        Just field -> return field

findFieldById :: Id -> DBMLMonad NField
findFieldById id = do
  state <- get
  let fieldMaybe = Map.lookup id (fieldS state)
  case fieldMaybe of
    Nothing ->
      lift (Left ("Cannot find field with id " `T.append` (T.pack . show) id))
    Just field -> return field

