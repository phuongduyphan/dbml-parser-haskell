{-# LANGUAGE OverloadedStrings #-}

module DBML.Exporter
  ( export
  , ExportTargetType (..)
  )
where

import           DBML.Normalizer                ( DBMLNormalizedState )
import           Data.Text                      ( Text )
import qualified DBML.Exporter.PostgreSQL      as Pg

data ExportTargetType = PostgreSQL | MySQL deriving (Show)

export :: DBMLNormalizedState -> ExportTargetType -> Text
export ns target = case target of
  PostgreSQL -> Pg.export ns
  _          -> ""
