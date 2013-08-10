
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Instruction
import Emulator
import MonadEmulator (LoadStore(..))

import Data.Monoid (All(..), getAll)
import Data.Word (Word64)
import Control.Monad (when, unless)
import Control.Monad.Writer (execWriterT, tell, WriterT)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import Data.Time (getZonedTime)
import System.IO (withFile, IOMode(..), hPutStrLn, Handle)

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

checkEmuTestResult ::
    String ->
    String ->
    Handle ->
    ([Cond], [Cond], [Cond], String, B.ByteString) ->
    WriterT All IO ()
checkEmuTestResult testName tracefn h (condSuccess, condFailure, condStop, cpust, trace) = do
    let resultStr = (if null condFailure then "Succeeded" else "Failed") ++ ":\n" ++
                    "    Stop Reason      "  ++ show condStop    ++ "\n" ++
                    "    Unmet Conditions "  ++ show condFailure ++ "\n" ++
                    "    Met Conditions   "  ++ show condSuccess ++ "\n"
    liftIO $ do
        hPutStrLn    h $ "--- " ++ testName ++ " ---\n"
        B8.hPutStrLn h trace
        hPutStrLn    h resultStr
    unless (null condFailure) $ do
        tell $ All False
        liftIO $ do
            putStrLn $ testName ++ " " ++ resultStr ++
                "    CPU State        "             ++ cpust   ++ "\n" ++
                "    Trace            Written to '" ++ tracefn ++ "'"

runTests :: IO Bool
runTests = do
    let tracefn = "./trace.log"
    withFile tracefn WriteMode $ \h -> do
        time <- getZonedTime
        hPutStrLn h $ "Trace Log " ++ show time ++ "\n"
        -- We use a writer with an All monoid so we can return False if any test fails
        w <- execWriterT $ do -- WriterT All IO ()
            -- Decoding test
            do
                bin <- liftIO $ B.readFile "./tests/instr_test.bin"
                ref <- liftIO $ B.readFile "./tests/instr_test_ref_disasm.asm"
                case decodingTest bin ref of
                    Left err -> (tell $ All False) >> (liftIO . putStrLn $ "Decoding Test Failed: " ++ err)
                    Right _  -> return ()
            -- Load / Store test
            do
                bin <- liftIO $ B.readFile "./tests/load_store_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x022A) (Left 0x55)
                                         , CondLS A (Left 0x55)
                                         , CondLS X (Left 0x2A)
                                         , CondLS Y (Left 0x73)
                                         , CondCycleR 161 161
                                         ]
                                         True
                checkEmuTestResult "Load / Store Test" tracefn h emures
            -- AND / OR / XOR test
            do
                bin <- liftIO $ B.readFile "./tests/and_or_xor_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x00A9) (Left 0xAA)
                                         , CondCycleR 332 332
                                         ]
                                         True
                checkEmuTestResult "AND / OR / XOR Test" tracefn h emures
            -- INC / DEC test
            do
                bin <- liftIO $ B.readFile "./tests/inc_dec_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0071) (Left 0xFF)
                                         , CondCycleR 149 149
                                         ]
                                         True
                checkEmuTestResult "INC / DEC Test" tracefn h emures
            -- Bitshift test
            do
                bin <- liftIO $ B.readFile "./tests/bitshift_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x01DD) (Left 0x6E)
                                         , CondCycleR 253 253
                                         ]
                                         True
                checkEmuTestResult "Bitshift Test" tracefn h emures

        return $ getAll w

disassemble :: B.ByteString -> [B.ByteString]
disassemble bin = do
    let vec = VU.fromList $ B.unpack bin
        disassemble' pc = do
            let instr   = decodeInstruction vec pc
                newPC   = pc + instructionLen instr
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

