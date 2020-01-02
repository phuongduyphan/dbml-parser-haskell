{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DBML
  ( pAlias
  , pFieldDefault
  , pRefInline
  , pIndexType
  , pNoteInline
  , pNameInline
  , pIndexSetting
  , pIndexSettings
  , pIndexIdentifier
  , pIndexIdentifiers
  , pIndex
  , pIndexes
  , pFieldSetting
  , pFieldSettings
  , pField
  , pEnumValueSetting
  , pEnumValueSettings
  , pEnumValue
  , pEnum
  , pTableValue
  , pTableSetting
  , pTable
  , DefaultType
  , Relation
  , IndexType
  , IndexSetting
  , IndexIdentifier
  , Index
  , FieldSetting
  , EnumValueSetting
  , EnumValue
  , DBML.Enum
  , Field
  , TableValue
  , TableSetting
  , Table
  )
where

import           Data.Text                      ( Text )
import           DBML.Lexer
import           DBML.Keyword
import           Control.Applicative
import           Text.Megaparsec.Char
import           Text.Megaparsec               as MP
import           Data.Scientific                ( Scientific )
import           Data.Maybe                     ( fromMaybe )

data DefaultType = DefaultString Text
  | DefaultExpr Expression
  | DefaultNum Scientific
  | DefaultBool Boolean
  | DefaultNull deriving (Show)

data Relation = OneToMany Char | ManyToOne Char | OneToOne Char deriving (Show)

data RefInline = RefInline
  { refTableName :: Text
  , refFieldName :: Text
  , refRelation :: Relation
  } deriving (Show)

data IndexType = BTree | Hash deriving (Show)

data IndexSetting = IndexPk | IndexUnique | IndexName Text | IndexType IndexType deriving (Show)

data IndexIdentifier = IndexColumn Text | IndexExpr Expression deriving (Show)

data Index = Index
  { indexIdentifiers :: [IndexIdentifier]
  , indexSettings :: Maybe [IndexSetting]
  } deriving (Show)

data FieldSetting = FieldNotNull
  | FieldNull 
  | FieldPk 
  | FieldUnique 
  | FieldIncrement 
  | FieldNote Text 
  | FieldRefInline RefInline
  | FieldDefault DefaultType deriving (Show)

newtype EnumValueSetting = EnumValueNote Text deriving (Show)

data EnumValue = EnumValue
  { enumValue :: Text
  , enumValueSettings :: Maybe [EnumValueSetting]
  } deriving (Show)

data Enum = Enum 
  { enumName :: Text
  , enumValues :: [EnumValue]
  } deriving (Show)

data Field = Field
 { fieldName :: Text
 , fieldType :: Text
 , fieldSettings :: Maybe [FieldSetting]
 } deriving (Show)

data TableValue = TableField Field | TableIndexes [Index] deriving (Show)

data TableSetting = TableHeaderColor Text | TableNote Text deriving (Show)

data Table = Table 
  { tableName :: Text 
  , tableSettings :: Maybe [TableSetting]
  , tableValues :: Maybe [TableValue]
  } deriving (Show)

pTable :: Parser Table
pTable = do
  table
  tableName <- identifier
  tableSettings <- optional pTableSettings
  lexeme (char '{')
  tableValues <- optional $ MP.many pTableValue
  lexeme (char '}')
  return Table {..}

pTableSettings :: Parser [TableSetting]
pTableSettings = do
  lexeme (char '[')
  fstTableSetting <- pTableSetting
  tableSettings   <- optional (MP.some (lexeme (char ',') *> pTableSetting))
  lexeme (char ']')
  return (fstTableSetting : fromMaybe [] tableSettings)

pTableSetting :: Parser TableSetting
pTableSetting = 
  (TableHeaderColor <$> lexeme (string' "headercolor"))
  <|> (TableNote <$> pNoteInline)

pTableValue :: Parser TableValue
pTableValue = try (TableField <$> pField) <|> (TableIndexes <$> pIndexes)

pField :: Parser Field
pField = do
  fieldName <- identifier
  fieldType <- identifier
  fieldSettings <- optional pFieldSettings
  return Field {..}

pFieldSettings :: Parser [FieldSetting]
pFieldSettings = do
  lexeme (char '[')
  fstFieldSetting <- pFieldSetting
  fieldSettings   <- optional (MP.some (lexeme (char ',') *> pFieldSetting))
  lexeme (char ']')
  return (fstFieldSetting : fromMaybe [] fieldSettings)

pFieldSetting :: Parser FieldSetting
pFieldSetting = 
  (FieldNotNull <$ (lexeme (string' "not") *> lexeme (string' "null")))
  <|> (FieldNull <$ lexeme (string' "null"))
  <|> (FieldPk <$ ((lexeme (string' "primary") *> lexeme (string' "key")) <|> lexeme (string' "pk")))
  <|> (FieldUnique <$ lexeme (string' "unique"))
  <|> (FieldIncrement <$ lexeme (string' "increment"))
  <|> (FieldNote <$> pNoteInline)
  <|> (FieldRefInline <$> pRefInline)
  <|> (FieldDefault <$> pFieldDefault)

pEnum :: Parser DBML.Enum
pEnum = do
  enum
  enumName <- identifier
  lexeme (char '{')
  enumValues <- MP.many pEnumValue
  lexeme (char '}')
  return Enum {..}

pEnumValue :: Parser EnumValue
pEnumValue = do
  enumValue <- identifier
  enumValueSettings <- optional pEnumValueSettings
  return EnumValue {..}

pEnumValueSettings :: Parser [EnumValueSetting]
pEnumValueSettings = do
  lexeme (char '[')
  fstEnumSetting <- pEnumValueSetting
  enumSettings   <- optional (MP.some (lexeme (char ',') *> pEnumValueSetting))
  lexeme (char ']')
  return (fstEnumSetting : fromMaybe [] enumSettings)

pEnumValueSetting :: Parser EnumValueSetting
pEnumValueSetting = EnumValueNote <$> pNoteInline

pIndexes :: Parser [Index]
pIndexes =
  indexes *> between (lexeme (char '{')) (lexeme (char '}')) (MP.many pIndex)

pIndex :: Parser Index
pIndex = do
  ms               <- optional . try $ pIndexIdentifier
  indexIdentifiers <- maybe pIndexIdentifiers (return . return) ms
  indexSettings    <- optional pIndexSettings
  return Index { .. }

pIndexIdentifiers :: Parser [IndexIdentifier]
pIndexIdentifiers = do
  lexeme (char '(')
  fstIdxIdentifier <- pIndexIdentifier
  idxIdentifiers   <- optional (MP.some (lexeme (char ',') *> pIndexIdentifier))
  lexeme (char ')')
  return (fstIdxIdentifier : fromMaybe [] idxIdentifiers)

pIndexIdentifier :: Parser IndexIdentifier
pIndexIdentifier =
  (IndexColumn <$> identifier) <|> (IndexExpr <$> expressionLiteral)

pIndexSettings :: Parser [IndexSetting]
pIndexSettings = do
  lexeme (char '[')
  fstIndexSetting <- pIndexSetting
  indexSettings   <- optional (MP.some (lexeme (char ',') *> pIndexSetting))
  lexeme (char ']')
  return (fstIndexSetting : fromMaybe [] indexSettings)

pIndexSetting :: Parser IndexSetting
pIndexSetting =
  (IndexPk <$ lexeme (string' "pk"))
    <|> (IndexUnique <$ lexeme (string' "unique"))
    <|> (IndexName <$> pNameInline)
    <|> (IndexType <$> pIndexType)

pHeaderColorInline :: Parser Text
pHeaderColorInline = lexeme (string' "headercolor") *> lexeme (char ':') *> stringLiteral

pNameInline :: Parser Text
pNameInline = lexeme (string' "name") *> lexeme (char ':') *> stringLiteral

pNoteInline :: Parser Text
pNoteInline = lexeme (string' "note") *> lexeme (char ':') *> stringLiteral

pIndexType :: Parser IndexType
pIndexType = lexeme (string' "type") *> lexeme (char ':') *> try
  ((BTree <$ lexeme (string "btree")) <|> (Hash <$ lexeme (string "hash")))

pRefInline :: Parser RefInline
pRefInline = do
  lexeme (string' "ref")
  lexeme (char ':')
  refRelation <-
    (OneToMany <$> lexeme (char '<'))
    <|> (ManyToOne <$> lexeme (char '>'))
    <|> (OneToOne <$> lexeme (char '-'))
  refTableName <- identifier
  lexeme (char '.')
  refFieldName <- identifier
  return RefInline { .. }

pFieldDefault :: Parser DefaultType
pFieldDefault =
  lexeme (string' "default")
    *> lexeme (char ':')
    *> (   (DefaultString <$> stringLiteral)
       <|> (DefaultExpr <$> expressionLiteral)
       <|> (DefaultBool <$> booleanLiteral)
       <|> (DefaultNum <$> numberLiteral)
       <|> (DefaultNull <$ lexeme (string' "null"))
       )

pAlias :: Parser Text
pAlias = as *> identifier
