{-# LANGUAGE OverloadedStrings #-}

module DBML.Keyword
  ( table
  , as
  , ref
  , indexes
  , enum
  , tableGroup
  )
where

import           DBML.Lexer
import           Data.Text                      ( Text )
import           Text.Megaparsec.Char
import           Control.Applicative

table :: Parser Text
table = lexeme $ string' "table"

as :: Parser Text
as = lexeme $ string' "as"

ref :: Parser Text
ref = lexeme $ string' "ref"

indexes :: Parser Text
indexes = lexeme $ string' "indexes"

enum :: Parser Text
enum = lexeme $ string' "enum"

tableGroup :: Parser Text
tableGroup = lexeme $ string' "tablegroup"