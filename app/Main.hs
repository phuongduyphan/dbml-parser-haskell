{-# LANGUAGE OverloadedStrings #-}

module Main where

import           DBML
import           Control.Monad.Trans.State.Lazy
import           Control.Monad                  ( guard )
import           Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Map.Strict               as Map
import           Text.Pretty.Simple             ( pPrint )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           System.Environment             ( getArgs )
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data Command = CmdParse Text | CmdNormalize Text | CmdExport CmdExportCommandType deriving (Show)

data CmdExportCommandType = CmdExportCommandType
  { cmdFilePath :: Text
  , postgresql :: Bool } deriving (Show)

pCmd :: Parser Command
pCmd = hsubparser
  (  command "parse" (info parseCmd (progDesc "parse DBML to AST"))
  <> command
       "normalize"
       (info normalizeCmd (progDesc "convert DBML AST to Map structure"))
  <> command
       "export"
       (info (CmdExport <$> exportCmd)
             (progDesc "export DBML to other database format")
       )
  )

parseCmd :: Parser Command
parseCmd = CmdParse <$> argument str (metavar "FILEPATH")

normalizeCmd :: Parser Command
normalizeCmd = CmdNormalize <$> argument str (metavar "FILEPATH")

exportCmd :: Parser CmdExportCommandType
exportCmd =
  CmdExportCommandType <$> argument str (metavar "FILEPATH") <*> switch
    (long "postgresql" <> help "PostgreSQL database format")

pCmdInfo :: ParserInfo Command
pCmdInfo = info (pCmd <**> helper) (fullDesc <> progDesc "DBML cli tool")

main :: IO ()
main = do
  cmd <- execParser pCmdInfo
  case cmd of
    CmdParse filepath -> do
      dbml <- readFile (T.unpack filepath)
      case parse (T.pack dbml) of
        Right db -> pPrint db
        Left  e  -> putStrLn (errorBundlePretty e)
    CmdNormalize filepath -> do
      dbml <- readFile (T.unpack filepath)
      case parse (T.pack dbml) of
        Right db -> case normalize db of
          Right ns -> pPrint ns
          Left e -> putStrLn $ T.unpack e
        Left  e  -> putStrLn (errorBundlePretty e)
    CmdExport cmdExport -> do
      dbml <- readFile (T.unpack (cmdFilePath cmdExport))
      if postgresql cmdExport then
        case parse (T.pack dbml) of
          Right db -> case normalize db of
            Right ns -> putStrLn $ T.unpack (export ns PostgreSQL)
            Left e -> putStrLn $ T.unpack e
          Left  e  -> putStrLn (errorBundlePretty e)
      else 
        putStrLn "Only support PostgreSQL for now"

-- dbml <- readFile fpath
--       case parse (T.pack dbml) of
--         Right db -> case normalize db of
--           Right ns -> putStrLn $ T.unpack (export ns PostgreSQL)
--           Left e -> putStrLn $ T.unpack e
--         Left e -> putStrLn (errorBundlePretty e)
