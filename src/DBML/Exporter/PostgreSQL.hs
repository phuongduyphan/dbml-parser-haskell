{-# LANGUAGE OverloadedStrings #-}

module DBML.Exporter.PostgreSQL
  ( export
  )
where

import           DBML.Normalizer
import           DBML.Parser                    ( EnumValue, enumValue )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad.Trans.Reader
import           Control.Monad                  ( unless )
import qualified Data.Map.Strict               as Map
import           Data.List                      ( intersperse )

export :: DBMLNormalizedState -> Text
export = runReader exportM

exportM :: Reader DBMLNormalizedState Text
exportM = exportEnums
  -- tables <- exportTables
  -- indexes <- exportIndexes
  -- refs <- exportRefs
  -- comments <- exportComments

exportEnums :: Reader DBMLNormalizedState Text
exportEnums = do
  ns    <- ask
  enums <- mapM (exportEnum .fst) (Map.toList (enumS ns))
  let enumStr = foldl T.append "" (intersperse "\n" enums)
  if not $ null enums then 
    return (enumStr `T.append` "\n")
  else 
    return enumStr

exportEnum :: Id -> Reader DBMLNormalizedState Text
exportEnum enumId = do
  ns <- ask
  case Map.lookup enumId (enumS ns) of
    Just enum -> 
      let enumValStr = foldl T.append "" (intersperse ",\n" (map exportEnumVal (neValues enum)))
      in return ("CREATE TYPE " `T.append` wrapInQuote "\"" (neName enum) `T.append` " AS ENUM (\n" `T.append` enumValStr `T.append` "\n);\n")
    Nothing -> return ""

exportEnumVal :: EnumValue -> Text
exportEnumVal enumVal = "  " `T.append` wrapInQuote "'" (enumValue enumVal)

wrapInQuote :: Text -> Text -> Text
wrapInQuote q s = q `T.append` s `T.append` q
