
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Test (runTests, TestMode(..))
import Instruction (disassemble)
import Emulator (TraceMode(..))

import Control.Monad (unless, forM_, void)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import Data.List (find)

data Flag = FlagTest String
          | FlagQuickTest
          | FlagDAsm String
          | FlagListTests String
          | FlagNoVerbose
          | FlagTraceOverride String
            deriving Eq

options :: [OptDescr Flag]
options = [ Option ['t']
                   ["test"]
                   (OptArg (FlagTest . fromMaybe "") "NAME")
                   "run full test suite / all tests containing NAME"
          , Option ['q']
                   ["quick-test"]
                   (NoArg FlagQuickTest)
                   "run quick (<1s) test suite"
          , Option ['l']
                   ["list-tests"]
                   (OptArg (FlagListTests . fromMaybe "") "NAME")
                   "list all tests / all tests containing NAME"
          , Option ['d']
                   ["dasm"]
                   (ReqArg FlagDAsm "FILE")
                   "disassemble binary FILE"
          , Option ['s']
                   ["no-verbose"]
                   (NoArg FlagNoVerbose)
                   "less flashy test result display"
          , Option ['r']
                   ["trace"]
                   (ReqArg FlagTraceOverride "[nbe]")
                   "override test tracing to [n]one, [b]asic, full [e]xecution"
          ]

main :: IO ()
main = do
    name <- getProgName
    args <- getArgs
    -- Parse command line arguments
    let header = "Usage: " ++ name ++ " [OPTION...]"
        usage  = usageInfo header options ++
                 "+RTS -? runtime system help, +RTS -N2 use two cores, +RTS -N use all cores\n"
    flags <- case getOpt Permute options args of
                 ([], [], [] ) -> putStrLn usage >> return []
                 (f , [], [] ) -> return f
                 (_ , _ , err) -> putStrLn (concat err ++ usage) >> return []
    -- Process arguments
    let lowVerbosity  = FlagNoVerbose `elem` flags
        traceOverride = let toFlag = find (\case FlagTraceOverride _ -> True; _ -> False) flags
                         in case toFlag of
                                Just (FlagTraceOverride "n") -> Just TraceNone
                                Just (FlagTraceOverride "b") -> Just TraceBasic
                                Just (FlagTraceOverride "e") -> Just TraceFullExe
                                _                            -> Nothing
    forM_ flags $ \case
        FlagTest tname      -> do success <- runTests TMAll tname lowVerbosity traceOverride
                                  unless success exitFailure
        FlagQuickTest       -> do success <- runTests TMQuick "" lowVerbosity traceOverride
                                  unless success exitFailure
        FlagListTests tname -> void $ runTests TMList tname lowVerbosity Nothing
        FlagDAsm fn         -> mapM_ B8.putStrLn . disassemble =<< B.readFile fn
        FlagNoVerbose       -> return ()
        FlagTraceOverride _ -> return ()
    exitSuccess

