{-# LANGUAGE OverloadedStrings #-}

module DBML
  ( DBML.parse
  , processTables
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
import           Data.Maybe                     ( fromMaybe )
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
  , tableIdCounter :: Id
  , enumIdCounter :: Id
  , refIdCounter :: Id
  , tableGroupIdCounter :: Id
  , fieldIdCounter :: Id
  } deriving (Show)

data NTable = NTable
  { ntId :: Id
  , ntGroupId :: Maybe Id
  , ntName :: Text
  , ntTableSettings :: Maybe [TableSetting]
  , ntFieldIds :: [Id]
  -- , ntIndexIds :: [Id]
  } deriving (Show)

data NEnum = NEnum
  { neId :: Id
  , neName :: Text
  , neValueIds :: [Id]
  , neFieldIds :: [Id]
  } deriving (Show)

data NRef = NRef
  { nrId :: Id
  , nrName :: Maybe Text
  , nrValueRelation :: RefRelation
  , nrValueSettings :: Maybe [RefSetting]
  , nrEndpointIds :: [Id]
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
  , niIndexSettings :: Maybe [IndexSetting]
  , niIndexIdentifiers :: Maybe [IndexIdentifier]
  } deriving (Show)

type DBMLMonad = StateT DBMLState (Either Text)

parse :: Text -> Either (ParseErrorBundle Text Void) Database
parse = MP.parse pDatabase ""

processTables :: Database -> DBMLMonad ()
processTables db = do
  tableMap <- getTableMap db
  state    <- get
  put state { tableS = Map.union (tableS state) tableMap }
  return ()

getTableMap :: Database -> DBMLMonad (Map.Map Id NTable)
getTableMap (Database xs) = Map.fromList <$> mapM buildTable tables
 where
  tables = filter isTable xs
  isTable (DBMLTable _) = True
  isTable _             = False
  buildTable (DBMLTable table) = do
    state <- get
    let tableId = tableIdCounter state
    put state { tableIdCounter = tableId + 1 }
    fieldMap <- getFieldMap table tableId
    case allDifferent (map (nfName . snd) (Map.toList fieldMap)) of
      Left fname -> lift
        (        Left
        $        "Field "
        `T.append` fname
        `T.append` " existed in table "
        `T.append` tableName table
        )
      _ -> lift (Right ())
    state1 <- get
    put state1 { fieldS = Map.union (fieldS state1) fieldMap }
    -- indexMap <- getIndexMap table (ntId nTable)
    -- put state { indexS = Map.union (indexS state) indexMap }
    return
      ( tableId
      , NTable { ntId            = tableIdCounter state
               , ntGroupId       = Nothing
               , ntName          = tableName table
               , ntTableSettings = tableSettings table
               , ntFieldIds      = map fst (Map.toList fieldMap)
               }
      )

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
    newState <- get
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
