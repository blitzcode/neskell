
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Test (runTests, TestMode(..))
import Instruction (disassemble)

import Control.Monad (when, unless, forM_, void)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Flag = FlagTest String | FlagQuickTest | FlagDAsm String | FlagListTests

options :: [OptDescr Flag]
options = [ Option ['t'] ["test"      ] (OptArg (FlagTest . fromMaybe "") "NAME")
                   "run full test suite / all tests containing NAME"
          , Option ['q'] ["quick-test"] (NoArg FlagQuickTest   ) "run quick (<1s) test suite"
          , Option ['d'] ["dasm"      ] (ReqArg FlagDAsm "FILE") "disassemble binary FILE"
          , Option ['l'] ["list-tests"] (NoArg FlagListTests   ) "list all tests by name"
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
    when (null flags) exitFailure
    -- Process arguments
    forM_ flags $ \case
        FlagTest tname -> do success <- runTests $ TMAll tname
                             unless success exitFailure
        FlagQuickTest  -> do success <- runTests TMQuick
                             unless success exitFailure
        FlagDAsm fn    -> mapM_ B8.putStrLn . disassemble =<< B.readFile fn
        FlagListTests  -> void $ runTests TMList
    exitSuccess

-- TODO: Add option to set tracing to none / basic / execution
-- TODO: Add option to control output verbosity (full ANSI / simple)

