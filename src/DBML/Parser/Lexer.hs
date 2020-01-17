{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DBML.Parser.Lexer
  ( sc
  , lexeme
  , stringLiteral
  , booleanLiteral
  , numberLiteral
  , expressionLiteral
  , identifier
  , typeLiteral
  , colorLiteral
  , Parser
  , FieldType
  , Expression
  , Color
  )
where

import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Char
import           Control.Applicative
import           Control.Applicative.Combinators
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Scientific                ( Scientific, toRealFloat )
import           Data.Void
import qualified Text.Megaparsec               as MP
                                         hiding ( State )

type Parser = MP.Parsec Void Text
type Expression = Text
data Color = RGB Int Int Int | RGBA Int Int Int Float | HexColor Text deriving (Show)
data FieldType = FieldType
  { typeName :: Text
  , typeConstraint :: Maybe Text
  } deriving (Show)

identifier :: Parser Text
identifier = variable <|> quotingVariable

typeLiteral :: Parser FieldType
typeLiteral =
  do
      typeName       <- variable
      typeConstraint <-
        optional
        $   T.pack
        <$> (lexeme (char '(') *> MP.manyTill L.charLiteral (lexeme $ char ')'))
      lexeme $ return FieldType { .. }
    <|> do
          typeName <- quotingVariable
          lexeme $ return (FieldType typeName Nothing)

colorLiteral :: Parser Color
colorLiteral = MP.try colorRGB <|> colorRGBA <|> hexColor

colorRGB :: Parser Color
colorRGB = do
  lexeme (string' "rgb")
  lexeme (char '(')
  val1 <- lexeme L.decimal
  lexeme (char ',')
  val2 <- lexeme L.decimal
  lexeme (char ',')
  val3 <- lexeme L.decimal
  lexeme (char ')')
  return (RGB val1 val2 val3)

colorRGBA :: Parser Color
colorRGBA = do
  lexeme (string' "rgba")
  lexeme (char '(')
  val1 <- lexeme L.decimal
  lexeme (char ',')
  val2 <- lexeme L.decimal
  lexeme (char ',')
  val3 <- lexeme L.decimal
  lexeme (char ',')
  val4 <- lexeme L.scientific
  lexeme (char ')')
  return (RGBA val1 val2 val3 (toRealFloat val4))

hexColor :: Parser Color
hexColor = HexColor <$> (lexeme (char '#') *> (T.pack <$> MP.some alphaNumChar))

variable :: Parser Text
variable = lexeme $ T.pack <$> MP.some (alphaNumChar <|> char '_')

quotingVariable :: Parser Text
quotingVariable =
  lexeme
    $   T.pack
    <$> (lexeme (char '"') *> MP.manyTill L.charLiteral (lexeme $ char '"'))

stringLiteral :: Parser Text
stringLiteral =
  lexeme
    $   T.pack
    <$> (lexeme (char '\'') *> MP.manyTill L.charLiteral (lexeme $ char '\''))

expressionLiteral :: Parser Expression
expressionLiteral =
  lexeme
    $   T.pack
    <$> (lexeme (char '`') *> MP.manyTill L.charLiteral (lexeme $ char '`'))

booleanLiteral :: Parser Bool
booleanLiteral =
  (True <$ lexeme (string "true"))
    <|> (False <$ lexeme (string "false"))

numberLiteral :: Parser Text
numberLiteral = lexeme $ T.pack <$> MP.some (alphaNumChar <|> char '-' <|>  char '+' <|> char '.' <|> char ',')

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
