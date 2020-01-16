{-# LANGUAGE OverloadedStrings #-}

module Main where

import           DBML
import           Control.Monad.Trans.State.Lazy
import           Control.Monad                  ( guard )
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Text.Pretty.Simple             ( pPrint )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  if Prelude.null args then
    putStrLn "please enter dbml file path"
  else
    let fpath = Prelude.head args in do
      dbml <- readFile fpath
      case parse (T.pack dbml) of
        Right db -> case normalize db of
          Right ns -> putStrLn $ T.unpack (export ns PostgreSQL)
          Left e -> putStrLn $ T.unpack e
        Left e -> putStrLn (errorBundlePretty e)
