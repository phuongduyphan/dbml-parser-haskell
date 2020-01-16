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
main = error ""
-- main = do
--   args <- getArgs
--   if Prelude.null args then
--     putStrLn "please enter dbml file path"
--   else
--     let fpath = Prelude.head args in do
--       dbml <- readFile fpath
--       case parse (T.pack dbml) of
--         Right db -> pPrint
--           (execStateT
--             (normalize db)
--             DBMLNormalizedState { tableS              = Map.empty
--                       , enumS               = Map.empty
--                       , refS                = Map.empty
--                       , tableGroupS         = Map.empty
--                       , fieldS              = Map.empty
--                       , indexS              = Map.empty
--                       , endpointS           = Map.empty
--                       , tableIdCounter      = 0
--                       , enumIdCounter       = 0
--                       , refIdCounter        = 0
--                       , tableGroupIdCounter = 0
--                       , fieldIdCounter      = 0
--                       , indexIdCounter      = 0
--                       , endpointIdCounter   = 0
--                       }
--           )
--         Left e -> putStrLn (errorBundlePretty e)
