
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Instruction as I
import qualified Emulator as E

import Data.Monoid (All(..), getAll)
import Control.Monad (when, unless)
import Control.Monad.Writer (execWriterT, tell)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)

decodingTest :: B.ByteString -> B.ByteString -> Either String ()
decodingTest bin ref = do
    let dasm = disassemble bin
        refl = B8.lines ref
        loop r d = do
            let safeR = case r of [] -> ""; (x:_) -> x
            let safeD = case d of [] -> ""; (x:_) -> x
            when (safeR /= safeD) . throwError $
                "Expected '" ++ B8.unpack safeR ++ "' got '" ++ B8.unpack safeD ++ "'"
            unless (null r || null d) $ loop (tail r) (tail d)
     in loop refl dasm

runTests :: IO Bool
runTests = do
    -- We use a writer with an All monoid so we can return False if any test fails
    w <- execWriterT $ do -- WriterT All IO ()
        -- Decoding test
        binInst <- liftIO $ B.readFile "./tests/instr_test.bin"
        refInst <- liftIO $ B.readFile "./tests/instr_test_ref_disasm.asm"
        case decodingTest binInst refInst of
            Left err -> (tell $ All False) >> (liftIO . putStrLn $ "Decoding Test Failed: " ++ err)
            Right _  -> return ()
        -- Load / Store test
        binLS <- liftIO $ B.readFile "./tests/load_store_test.bin"
        let res = E.runEmulator binLS 0x0600 0x0600 $ E.TermOnOpC (I.OpCode I.BRK I.Implied)
        return ()
    return $ getAll w

disassemble :: B.ByteString -> [B.ByteString]
disassemble bin = do
    let vec = VU.fromList $ B.unpack bin
        disassemble' pc = do
            let instr   = I.decodeInstruction vec pc
                newPC   = pc + I.instructionLen instr
                showI   = B8.pack . show $ instr
                validPC = newPC < VU.length vec
            -- Build result using : instead of ++, no stack overflow etc.
            if validPC then showI : disassemble' newPC else [showI]
     in disassemble' 0

main :: IO ()
main = do
    name <- getProgName
    args <- getArgs
    when (null args) . putStrLn $ "Usage: " ++ name ++ " [--test] [--dasm file]"
    when ("--test" `elem` args) $ do
        success <- runTests
        unless success exitFailure
        putStrLn "All Tests OK"
    case dropWhile (\x -> x /= "--dasm") args of
        (_:fn:_) -> B.readFile fn >>= mapM_ (B8.putStrLn) . disassemble
        (_:_)    -> putStrLn "Missing file argument to --dasm" >> exitFailure
        []       -> return ()
    exitSuccess

