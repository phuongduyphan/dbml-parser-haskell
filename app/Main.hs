{-# LANGUAGE OverloadedStrings #-}

module Main where

import           DBML
import           Control.Monad.Trans.State.Lazy
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

main :: IO ()
main = do
  dbml <- getLine
  case parse (T.pack dbml) of
    Right db -> print (execStateT (normalize db) DBMLState 
      { tableS = Map.empty
      , enumS = Map.empty
      , refS = Map.empty
      , tableGroupS = Map.empty
      , fieldS = Map.empty
      , indexS = Map.empty
      , tableIdCounter = 0
      , enumIdCounter = 0
      , refIdCounter = 0
      , tableGroupIdCounter = 0
      , fieldIdCounter = 0
      , indexIdCounter = 0
      })
    Left e -> print e 
