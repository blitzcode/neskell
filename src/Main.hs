
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Test (runTests, TestMode(..))
import Instruction (disassemble)

import Control.Monad (unless, forM_, void)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Flag = FlagTest String | FlagQuickTest | FlagDAsm String | FlagListTests | FlagNoVerbose
            deriving Eq

options :: [OptDescr Flag]
options = [ Option ['t'] ["test"      ] (OptArg (FlagTest . fromMaybe "") "NAME")
                   "run full test suite / all tests containing NAME"
          , Option ['q'] ["quick-test"] (NoArg FlagQuickTest   ) "run quick (<1s) test suite"
          , Option ['d'] ["dasm"      ] (ReqArg FlagDAsm "FILE") "disassemble binary FILE"
          , Option ['l'] ["list-tests"] (NoArg FlagListTests   ) "list all tests by name"
          , Option ['s'] ["no-verbose"] (NoArg FlagNoVerbose   ) "less flashy test result display"
          ]

main :: IO ()
main = do
    name <- getProgName
    args <- getArgs
    -- Parse command line arguments
    let header = "Usage: " ++ name ++ " [OPTION...]"
        usage  = usageInfo header options
    flags <- case getOpt Permute options args of
                 ([], [], [] ) -> putStrLn usage >> return []
                 (f , [], [] ) -> return f
                 (_ , _ , err) -> putStrLn (concat err ++ usage) >> return []
    -- Process arguments
    let lowVerbosity = FlagNoVerbose `elem` flags
    forM_ flags $ \case
        FlagTest tname -> do success <- runTests (TMAll tname) lowVerbosity
                             unless success exitFailure
        FlagQuickTest  -> do success <- runTests TMQuick lowVerbosity
                             unless success exitFailure
        FlagDAsm fn    -> mapM_ B8.putStrLn . disassemble =<< B.readFile fn
        FlagListTests  -> void $ runTests TMList lowVerbosity
        FlagNoVerbose  -> return ()
    exitSuccess

-- TODO: Add option to set tracing to none / basic / execution

