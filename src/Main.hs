
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Instruction

import Control.Monad (when)
import Control.Monad.Error (throwError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as VU

decodingTest :: B.ByteString -> B.ByteString -> Either String ()
decodingTest bin ref = do
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

runTests :: IO ()
runTests = do
    -- Decoding test
    bin <- B.readFile "./tests/instr_test.bin"
    ref <- B.readFile "./tests/instr_test_ref_disasm.asm"
    putStrLn $ "Decoding Test " ++ case decodingTest bin ref of
        Left errStr -> "Failed: " ++ errStr
        Right _     -> "Succeeded"

main :: IO ()
main = do
    runTests

