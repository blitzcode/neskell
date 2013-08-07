
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Instruction
import CPU

import Data.Monoid (All(..), getAll)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, tell)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

decodingTest :: B.ByteString -> B.ByteString -> Either String ()
decodingTest bin ref = do
    -- Try to decode everything from the binary and compare line-by-line to the
    -- reference disassembly from ref, returns nothing or a diff string on error
    let vec = VU.fromList $ B.unpack bin
        disassemble pc refLine = do
            let instr   = decodeInstruction vec pc
                newPC   = pc + instructionLen instr
                showI   = show $ instr
                safeRef = B8.unpack $ case refLine of [] -> ""; (x:_) -> x
                match   = showI == safeRef
                validPC = newPC < VU.length vec
            when (not match) . throwError $ "Expected '" ++ safeRef ++ "' got '" ++ showI ++ "'"
            when validPC . disassemble newPC . tail $ refLine
     in disassemble 0 $ B8.lines ref

runTests :: IO Bool
runTests = do
    -- We use a writer with an All monoid so we can return False if any test fails
    w <- execWriterT $ do -- WriterT All IO ()
        -- Decoding test
        bin <- liftIO $ B.readFile "./tests/instr_test.bin"
        ref <- liftIO $ B.readFile "./tests/instr_test_ref_disasm.asm"
        case decodingTest bin ref of
            Left err -> (tell $ All False) >> (liftIO . putStrLn $ "Decoding Test " ++ "Failed: " ++ err)
            Right _  ->                        liftIO . putStrLn $ "Decoding Test " ++ "Succeeded"
    return $ getAll w

main :: IO ()
main = do
    args <- getArgs
    when ("--test" `elem` args) $ do
        success <- runTests
        if (success) then exitSuccess else exitFailure

