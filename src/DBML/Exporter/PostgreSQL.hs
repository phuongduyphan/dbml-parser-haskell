{-# LANGUAGE OverloadedStrings #-}

module DBML.Exporter.PostgreSQL
  ( export
  )
where

import           DBML.Normalizer
import           DBML.Parser                    ( EnumValue(..)
                                                , FieldSetting(..)
                                                , DefaultType(..)
                                                , IndexSetting(..)
                                                , IndexIdentifier(..)
                                                , RefRelation(..)
                                                , RefSetting(..)
                                                , RefAction(..)
                                                , IndexType(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad.Trans.Reader
import           Control.Monad                  ( unless )
import qualified Data.Map.Strict               as Map
import           Data.List                      ( intersperse
                                                , find
                                                )
import           Data.Maybe                     ( fromMaybe )

export :: DBMLNormalizedState -> Text
export = runReader exportM

exportM :: Reader DBMLNormalizedState Text
exportM = do
  enums  <- exportEnums
  tables <- exportTables
  refs <- exportRefs
  indexes <- exportIndexes
  -- comments <- exportComments
  return (enums `T.append` tables `T.append` refs `T.append` indexes)

exportEnums :: Reader DBMLNormalizedState Text
exportEnums = do
  ns    <- ask
  enums <- mapM (exportEnum . fst) (Map.toList (enumS ns))
  let enumStr = foldl T.append "" (intersperse "\n" enums)
  if not $ null enums then return (enumStr `T.append` "\n") else return enumStr

exportEnum :: Id -> Reader DBMLNormalizedState Text
exportEnum enumId = do
  ns <- ask
  case Map.lookup enumId (enumS ns) of
    Just enum ->
      let enumValStr = foldl
            T.append
            ""
            (intersperse ",\n" (map exportEnumVal (neValues enum)))
      in  return
            (          "CREATE TYPE "
            `T.append` wrapInQuote "\"" (neName enum)
            `T.append` " AS ENUM (\n"
            `T.append` enumValStr
            `T.append` "\n);\n"
            )
    Nothing -> return ""

exportEnumVal :: EnumValue -> Text
exportEnumVal enumVal = "  " `T.append` wrapInQuote "'" (enumValue enumVal)

wrapInQuote :: Text -> Text -> Text
wrapInQuote q s = q `T.append` s `T.append` q

exportTables :: Reader DBMLNormalizedState Text
exportTables = do
  ns     <- ask
  tables <- mapM (exportTable . fst) (Map.toList (tableS ns))
  let tableStr = foldl T.append "" (intersperse "\n" tables)
  if not $ null tables
    then return (tableStr `T.append` "\n")
    else return tableStr

exportTable :: Id -> Reader DBMLNormalizedState Text
exportTable tableId = do
  ns <- ask
  case Map.lookup tableId (tableS ns) of
    Just table -> do
      fieldLines   <- getFieldLines tableId
      compositePks <- getCompositePks tableId
      let tableValStr =
            foldl T.append "" (intersperse ",\n" (fieldLines ++ compositePks))
      return
        (          "CREATE TABLE "
        `T.append` ntName table
        `T.append` " (\n"
        `T.append` tableValStr
        `T.append` "\n);\n"
        )
    Nothing -> return ""

getFieldLines :: Id -> Reader DBMLNormalizedState [Text]
getFieldLines tableId = do
  ns <- ask
  let tableMaybe = Map.lookup tableId (tableS ns)
  case tableMaybe of
    Just table -> mapM getFieldLine (ntFieldIds table)
    Nothing    -> return []

getFieldLine :: Id -> Reader DBMLNormalizedState Text
getFieldLine fieldId = do
  ns <- ask
  let fieldMaybe = Map.lookup fieldId (fieldS ns)
  case fieldMaybe of
    Just field -> return
      (          "  "
      `T.append` exportFieldType
      `T.append` exportFieldUnique
      `T.append` exportFieldPk
      `T.append` exportFieldNotNull
      `T.append` exportFieldDefault
      )
     where
      exportFieldType
        | any isIncrement (fromMaybe [] (nfFieldSettings field))
        = wrapInQuote "\"" (nfName field) `T.append` " SERIAL"
        | T.isInfixOf " " (nfType field)
        = wrapInQuote "\"" (nfName field) `T.append` " " `T.append` wrapInQuote
          "\""
          (nfType field)
        | otherwise
        = wrapInQuote "\"" (nfName field) `T.append` " " `T.append` nfType field
       where
        isIncrement FieldIncrement = True
        isIncrement _              = False
      exportFieldUnique
        | any isUnique (fromMaybe [] (nfFieldSettings field)) = " UNIQUE"
        | otherwise = ""
       where
        isUnique FieldUnique = True
        isUnique _           = False
      exportFieldPk
        | any isPk (fromMaybe [] (nfFieldSettings field)) = " PRIMARY KEY"
        | otherwise = ""
       where
        isPk FieldPk = True
        isPk _       = False
      exportFieldNotNull
        | any isNotNull (fromMaybe [] (nfFieldSettings field)) = " NOT NULL"
        | otherwise = ""
       where
        isNotNull FieldNotNull = True
        isNotNull _            = False
      exportFieldDefault = case defaultSettingMaybe of
        Nothing                -> ""
        Just (FieldDefault fs) -> case fs of
          (DefaultString value) -> " DEFAULT " `T.append` wrapInQuote "'" value
          (DefaultExpr value) -> " DEFAULT (" `T.append` value `T.append` ")"
          (DefaultBool value) -> " DEFAULT " `T.append` (T.pack . show $ value)
          (DefaultNum value) -> " DEFAULT " `T.append` value
          DefaultNull -> " DEFAULT NULL"
        where
          defaultSettingMaybe =
            find isDefault (fromMaybe [] (nfFieldSettings field))
          isDefault (FieldDefault _) = True
          isDefault _                = False
    Nothing -> return ""

getCompositePks :: Id -> Reader DBMLNormalizedState [Text]
getCompositePks tableId = do
  ns <- ask
  let tableMaybe = Map.lookup tableId (tableS ns)
  case tableMaybe of
    Just table -> mapM getCompositePk (filter isCompositePk (ntIndexIds table))
     where
      isCompositePk indexId = case Map.lookup indexId (indexS ns) of
        Nothing    -> False
        Just index -> maybe False isIdxSettingPk (niIndexSettings index)
         where
          isIdxSettingPk = any isIndexPk
          isIndexPk IndexPk = True
          isIndexPk _       = False
    Nothing -> return []

getCompositePk :: Id -> Reader DBMLNormalizedState Text
getCompositePk indexId = do
  ns <- ask
  let indexMaybe = Map.lookup indexId (indexS ns)
  case indexMaybe of
    Just index -> return
      ("  PRIMARY KEY (" `T.append` exportColumns `T.append` ")")
     where
      exportColumns = foldl
        T.append
        ""
        (intersperse ", " (map exportColumn (niIndexIdentifiers index)))
      exportColumn (IndexColumn column) = wrapInQuote "\"" column
      exportColumn (IndexExpr   expr  ) = "(" `T.append` expr `T.append` ")"
    Nothing -> return ""

exportRefs :: Reader DBMLNormalizedState Text
exportRefs = do
  ns   <- ask
  refs <- mapM (exportRef . fst) (Map.toList (refS ns))
  let refStr = foldl T.append "" (intersperse "\n" refs)
  if not $ null refs then return (refStr `T.append` "\n") else return refStr

exportRef :: Id -> Reader DBMLNormalizedState Text
exportRef refId = do
  ns <- ask
  case Map.lookup refId (refS ns) of
    Just ref -> case nrValueRelation ref of
      OneToMany _ ->
        return $ fromMaybe "" (exportRefEndpoints (fst endpointIds) (snd endpointIds))
      ManyToOne _ ->
        return $ fromMaybe "" (exportRefEndpoints (snd endpointIds) (fst endpointIds))
      OneToOne _ ->
        return $ fromMaybe "" (exportRefEndpoints (fst endpointIds) (snd endpointIds))
     where
      endpointIds = nrEndpointIds ref
      exportRefEndpoints endpointId foreignEndpointId = do
        endpoint <- Map.lookup endpointId (endpointS ns)
        foreignEndpoint <- Map.lookup foreignEndpointId (endpointS ns)
        return
          ( "ALTER TABLE "
          `T.append` wrapInQuote "\"" (nepTableName foreignEndpoint)
          `T.append` " ADD"
          `T.append` exportRefName
          `T.append` " FOREIGN KEY ("
          `T.append` wrapInQuote "\"" (nepFieldName foreignEndpoint)
          `T.append` ") REFERENCES "
          `T.append` wrapInQuote "\"" (nepTableName endpoint)
          `T.append` " ("
          `T.append` wrapInQuote "\"" (nepFieldName endpoint)
          `T.append` ")"
          `T.append` exportRefSetting
          `T.append` ";\n" )
      exportRefName = case nrName ref of
        Just name -> " CONSTRAINT " `T.append` wrapInQuote "\"" name
        Nothing -> ""
      exportRefSetting = case nrValueSettings ref of
        Just refSettings -> exportRefOnDelete `T.append` exportRefOnUpdate
          where 
            exportRefOnDelete = case refOnDelete of
              Just (RefOnDelete refAction) -> " ON DELETE " `T.append` exportRefAction refAction
              Nothing -> ""
            exportRefOnUpdate = case refOnUpdate of
              Just (RefOnUpdate refAction) -> " ON UPDATE " `T.append` exportRefAction refAction
              Nothing -> ""
            exportRefAction NoAction = " NO ACTION"
            exportRefAction Restrict = " RESTRICT"
            exportRefAction Cascade = " CASCADE"
            exportRefAction SetNull = " SET NULL"
            exportRefAction SetDefault = " SET DEFAULT"
            refOnDelete = 
              find isRefOnDelete refSettings
              where 
                isRefOnDelete (RefOnDelete _) = True
                isRefOnDelete _ = False;
            refOnUpdate = 
              find isRefOnUpdate refSettings
              where 
                isRefOnUpdate (RefOnUpdate _) = True
                isRefOnUpdate _ = False;
        Nothing -> ""
    Nothing -> return ""

exportIndexes :: Reader DBMLNormalizedState Text
exportIndexes = do
  ns <- ask
  indexes <- mapM (exportIndex . fst) (Map.toList (indexS ns))
  let indexStr = foldl T.append "" (intersperse "\n" indexes)
  if not $ null indexes then return (indexStr `T.append` "\n") else return indexStr

exportIndex :: Id -> Reader DBMLNormalizedState Text
exportIndex indexId = do
  ns <- ask
  case Map.lookup indexId (indexS ns) of
    Just index -> return $ fromMaybe "" exportIndexMaybe
      where
        exportIndexMaybe = do
          table <- Map.lookup (niTableId index) (tableS ns)
          return 
            ( "CREATE"
            `T.append` exportIndexUnique
            `T.append` " INDEX"
            `T.append` exportIndexName
            `T.append` " ON "
            `T.append` wrapInQuote "\"" (ntName table)
            `T.append` exportIndexType
            `T.append` " ("
            `T.append` exportIndexIdentifiers 
            `T.append` ");\n")
        exportIndexUnique 
          | any isIndexUnique (fromMaybe [] (niIndexSettings index))
          = " UNIQUE"
          | otherwise = ""
          where 
            isIndexUnique IndexUnique = True
            isIndexUnique _ = False
        exportIndexName = case idxNameSetting of
          Nothing -> ""
          Just (IndexName name) -> " " `T.append` wrapInQuote "\"" name
          where 
            idxNameSetting = find isIndexName (fromMaybe [] (niIndexSettings index))
            isIndexName (IndexName _) = True
            isIndexName _ = False
        exportIndexType = case idxTypeSetting of
          Nothing -> ""
          Just (IndexType BTree) -> " USING BTREE"
          Just (IndexType Hash) -> " USING HASH"
          where
            idxTypeSetting = find isIndexType (fromMaybe [] (niIndexSettings index))
            isIndexType (IndexType _) = True
            isIndexType _ = False
        exportIndexIdentifiers = foldl T.append "" (map exportIndexIdentifier (niIndexIdentifiers index))
        exportIndexIdentifier (IndexColumn value) = wrapInQuote "\"" value
        exportIndexIdentifier (IndexExpr expr) = "(" `T.append` expr `T.append` ")"
    Nothing -> return ""