
module Main (main) where

import Test (runTests)
import Instruction (disassemble)

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
    -- TODO: This is a rather poor argument parser...
    name <- getProgName
    args <- getArgs
    when (null args) . putStrLn $ "Usage: " ++ name ++ " --[test|quick-test] [--dasm file]"
    when ("--test" `elem` args) $ do
        success <- runTests False
        unless success exitFailure
    when ("--quick-test" `elem` args) $ do
        success <- runTests True
        unless success exitFailure
    case dropWhile (/= "--dasm") args of
        (_:fn:_) -> B.readFile fn >>= mapM_ (B8.putStrLn) . disassemble
        (_:_)    -> putStrLn "Missing file argument to --dasm" >> exitFailure
        []       -> return ()
    exitSuccess

