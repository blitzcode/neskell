
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Test (runTests)
import Instruction (disassemble)

import Control.Monad (when, unless, forM_)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt

data Flag = FlagTest | FlagQuickTest | FlagDAsm String

options :: [OptDescr Flag]
options = [ Option ['t'] ["test"      ] (NoArg FlagTest        ) "run full test suite"
          , Option ['q'] ["quick-test"] (NoArg FlagQuickTest   ) "run quick (<1s) test suite"
          , Option ['d'] ["dasm"      ] (ReqArg FlagDAsm "FILE") "disassemble binary FILE"
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
        FlagTest      -> do success <- runTests False
                            unless success exitFailure
        FlagQuickTest -> do success <- runTests True
                            unless success exitFailure
        FlagDAsm fn   -> mapM_ B8.putStrLn . disassemble =<< B.readFile fn
    exitSuccess

-- TODO: Add option to only run tests matching a given regex, option to list
--       all tests by name

