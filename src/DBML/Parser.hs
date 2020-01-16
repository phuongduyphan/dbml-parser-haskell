{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DBML.Parser
  ( DBML.Parser.parse
  , Database(..)
  , Element(..)
  , Table(..)
  , DBML.Parser.Enum(..)
  , Ref(..)
  , TableGroup(..)
  , TableSetting
  , TableValue(..)
  , RefRelation
  , RefSetting
  , FieldSetting(..)
  , IndexSetting
  , IndexIdentifier(..)
  , Field(..)
  , Index(..)
  , EnumValue(..)
  , TableGroupValue(..)
  , RefValue(..)
  , RefEndpoint(..)
  , RefInline(..)
  )
where

import           Data.Text                      ( Text )
import           DBML.Parser.Lexer
import           DBML.Parser.Keyword
import           Control.Applicative
import           Text.Megaparsec.Char
import           Text.Megaparsec               as MP
import           Data.Scientific                ( Scientific )
import           Data.Maybe                     ( fromMaybe )
import           Data.Void                      ( Void )

data DefaultType = DefaultString Text
  | DefaultExpr Expression
  | DefaultNum Scientific
  | DefaultBool Boolean
  | DefaultNull deriving (Show)

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

data TableSetting = TableHeaderColor Color | TableNote Text deriving (Show)

data Table = Table
  { tableName :: Text
  , tableSettings :: Maybe [TableSetting]
  , tableValues :: Maybe [TableValue]
  } deriving (Show)

data RefAction = NoAction | Restrict | Cascade | SetNull | SetDefault deriving (Show)

data RefSetting = RefOnUpdate RefAction | RefOnDelete RefAction deriving (Show)

data RefEndpoint = RefEndpoint
  { endpointTableName :: Text
  , endpointFieldName :: Text
  } deriving (Show)

data RefValue = RefValue
  { refValueEndpoints :: [RefEndpoint]
  , refValueRelation :: RefRelation
  , refValueSettings :: Maybe [RefSetting]
  } deriving (Show)

data Ref = Ref
  { refName :: Maybe Text
  , refValue :: RefValue
  } deriving (Show)

data RefRelation = OneToMany Char | ManyToOne Char | OneToOne Char deriving (Show)

data RefInline = RefInline
  { refInlineTableName :: Text
  , refInlineFieldName :: Text
  , refInlineRelation :: RefRelation
  } deriving (Show)

newtype TableGroupValue = TableGroupValue { tgTableName :: Text } deriving (Show)

data TableGroup = TableGroup
  { tableGroupName :: Text
  , tableGroupValues :: [TableGroupValue]
  } deriving (Show)

data Element = DBMLTable Table | DBMLEnum DBML.Parser.Enum | DBMLRef Ref | DBMLTableGroup TableGroup deriving (Show)

newtype Database = Database [Element] deriving (Show)

parse :: Text -> Either (ParseErrorBundle Text Void) Database
parse = MP.parse pDatabase ""

pDatabase :: Parser Database
pDatabase = sc *> (Database <$> MP.many pElement <* eof)

pElement :: Parser Element
pElement =
  DBMLTable
    <$> try pTable
    <|> DBMLEnum
    <$> pEnum
    <|> DBMLRef
    <$> pRef
    <|> DBMLTableGroup
    <$> pTableGroup

pTableGroup :: Parser TableGroup
pTableGroup = do
  tableGroup
  tableGroupName <- identifier
  lexeme (char '{')
  tableGroupValues <- MP.many pTableGroupValue
  lexeme (char '}')
  return TableGroup { .. }

pTableGroupValue :: Parser TableGroupValue
pTableGroupValue = do
  tgTableName <- identifier
  return TableGroupValue { .. }

pRef :: Parser Ref
pRef = try pRefLong <|> pRefShort

pRefLong :: Parser Ref
pRefLong = do
  ref
  refName <- optional identifier
  lexeme (char '{')
  refValue <- pRefValue
  lexeme (char '}')
  return Ref { .. }

pRefShort :: Parser Ref
pRefShort = do
  ref
  refName <- optional identifier
  lexeme (char ':')
  refValue <- pRefValue
  return Ref { .. }

pRefValue :: Parser RefValue
pRefValue = do
  refEndpoint1 <- pRefEndpoint
  refRelation  <- pRefRelation
  refEndpoint2 <- pRefEndpoint
  refSettings  <- optional pRefSettings
  return (RefValue [refEndpoint1, refEndpoint2] refRelation refSettings)

pRefEndpoint :: Parser RefEndpoint
pRefEndpoint = do
  endpointTableName <- identifier
  lexeme (char '.')
  endpointFieldName <- identifier
  return RefEndpoint { .. }

pRefSettings :: Parser [RefSetting]
pRefSettings = do
  lexeme (char '[')
  fstRefSetting <- pRefSetting
  refSettings   <- optional (MP.some (lexeme (char ',') *> pRefSetting))
  lexeme (char ']')
  return (fstRefSetting : fromMaybe [] refSettings)

pRefSetting :: Parser RefSetting
pRefSetting =
  (   RefOnUpdate
    <$> (lexeme (string' "update") *> lexeme (char ':') *> pRefAction)
    )
    <|> (   RefOnDelete
        <$> (lexeme (string' "delete") *> lexeme (char ':') *> pRefAction)
        )

pRefAction :: Parser RefAction
pRefAction =
  (NoAction <$ (lexeme (string' "no") *> lexeme (string' "action")))
    <|> (Restrict <$ lexeme (string' "restrict"))
    <|> (Cascade <$ lexeme (string' "cascade"))
    <|> (SetNull <$ (lexeme (string' "set") *> lexeme (string' "null")))
    <|> (SetDefault <$ (lexeme (string' "set") *> lexeme (string' "default")))

pTable :: Parser Table
pTable = do
  table
  tableName     <- identifier
  tableSettings <- optional pTableSettings
  lexeme (char '{')
  tableValues <- optional $ MP.many pTableValue
  lexeme (char '}')
  return Table { .. }

pTableSettings :: Parser [TableSetting]
pTableSettings = do
  lexeme (char '[')
  fstTableSetting <- pTableSetting
  tableSettings   <- optional (MP.some (lexeme (char ',') *> pTableSetting))
  lexeme (char ']')
  return (fstTableSetting : fromMaybe [] tableSettings)

pTableSetting :: Parser TableSetting
pTableSetting =
  (   TableHeaderColor
    <$> (lexeme (string' "headercolor") *> lexeme (char ':') *> colorLiteral)
    )
    <|> (TableNote <$> pNoteInline)

pTableValue :: Parser TableValue
pTableValue = try (TableField <$> pField) <|> (TableIndexes <$> pIndexes)

pField :: Parser Field
pField = do
  fieldName     <- identifier
  fieldType     <- identifier
  fieldSettings <- optional pFieldSettings
  return Field { .. }

pFieldSettings :: Parser [FieldSetting]
pFieldSettings = do
  lexeme (char '[')
  fstFieldSetting <- pFieldSetting
  fieldSettings   <- optional (MP.some (lexeme (char ',') *> pFieldSetting))
  lexeme (char ']')
  return (fstFieldSetting : fromMaybe [] fieldSettings)

pFieldSetting :: Parser FieldSetting
pFieldSetting =
  (FieldNotNull <$ try (lexeme (string' "not") *> lexeme (string' "null")))
    <|> (FieldNull <$ lexeme (string' "null"))
    <|> (FieldPk <$ try
          (   (lexeme (string' "primary") *> lexeme (string' "key"))
          <|> lexeme (string' "pk")
          )
        )
    <|> (FieldUnique <$ lexeme (string' "unique"))
    <|> (FieldIncrement <$ lexeme (string' "increment"))
    <|> (FieldNote <$> pNoteInline)
    <|> (FieldRefInline <$> pRefInline)
    <|> (FieldDefault <$> pFieldDefault)

pEnum :: Parser DBML.Parser.Enum
pEnum = do
  enum
  enumName <- identifier
  lexeme (char '{')
  enumValues <- MP.many pEnumValue
  lexeme (char '}')
  return Enum { .. }

pEnumValue :: Parser EnumValue
pEnumValue = do
  enumValue         <- identifier
  enumValueSettings <- optional pEnumValueSettings
  return EnumValue { .. }

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
pHeaderColorInline =
  lexeme (string' "headercolor") *> lexeme (char ':') *> stringLiteral

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
  refInlineRelation  <- pRefRelation
  refInlineTableName <- identifier
  lexeme (char '.')
  refInlineFieldName <- identifier
  return RefInline { .. }

pRefRelation :: Parser RefRelation
pRefRelation =
  (OneToMany <$> lexeme (char '<'))
    <|> (ManyToOne <$> lexeme (char '>'))
    <|> (OneToOne <$> lexeme (char '-'))

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
