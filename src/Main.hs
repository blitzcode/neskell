
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Instruction (Mnemonic(..), decodeInstruction, instructionLen)
import Emulator (runEmulator, Cond(..))
import MonadEmulator (LoadStore(..))
import Util (srFromString)

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
                                         , CondLS SR (Left $ srFromString "N-1--I--")
                                         , CondLS A (Left 0xAA)
                                         , CondLS X (Left 0x10)
                                         , CondLS Y (Left 0xF0)
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
                                         , CondLS SR (Left $ srFromString "N-1--I--")
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
                                         , CondLS A (Left 0xDD)
                                         , CondLS X (Left 0xDD)
                                         , CondCycleR 253 253
                                         ]
                                         True
                checkEmuTestResult "Bitshift Test" tracefn h emures
            -- JMP/JSR/RTS test
            do
                bin <- liftIO $ B.readFile "./tests/jump_ret_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0040) (Left 0x42)
                                         , CondLS A (Left 0x42)
                                         , CondLS X (Left 0x33)
                                         , CondLS PC (Right 0x0626)
                                         , CondLS SP (Left 0xFF)
                                         , CondCycleR 50 50
                                         ]
                                         True
                checkEmuTestResult "JMP/JSR/RTS Test" tracefn h emures
            -- Register Transfer test
            do
                bin <- liftIO $ B.readFile "./tests/reg_transf_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0040) (Left 0x33)
                                         , CondLS A (Left 0x33)
                                         , CondLS X (Left 0x33)
                                         , CondLS Y (Left 0x33)
                                         , CondLS SP (Left 0x33)
                                         , CondCycleR 37 37
                                         ]
                                         True
                checkEmuTestResult "Register Transfer Test" tracefn h emures
            -- Add / Sub test
            do
                bin <- liftIO $ B.readFile "./tests/add_sub_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0030) (Left 0x9D)
                                         , CondLS SR (Left $ srFromString "NV1--I-C")
                                         , CondLS A (Left 0x9D)
                                         , CondLS X (Left 0x31)
                                         , CondLS Y (Left 0x16)
                                         , CondCycleR 203 203
                                         ]
                                         True
                checkEmuTestResult "Add / Sub Test" tracefn h emures
            -- BCD Add / Sub test
            do
                bin <- liftIO $ B.readFile "./tests/bcd_add_sub_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x01FF) (Left 0x05)
                                         , CondLS (Addr 0x01FE) (Left 0x46)
                                         , CondLS (Addr 0x01FD) (Left 0x41)
                                         , CondLS (Addr 0x01FC) (Left 0x73)
                                         , CondLS (Addr 0x01FB) (Left 0x34)
                                         , CondLS (Addr 0x01FA) (Left 0x27)
                                         , CondLS (Addr 0x01F9) (Left 0x29)
                                         , CondLS (Addr 0x01F8) (Left 0x91)
                                         , CondLS (Addr 0x01F7) (Left 0x87)
                                         , CondLS SP (Left 0xF6)
                                         , CondLS SR (Left $ srFromString "N-1-DI--")
                                         , CondCycleR 73 73
                                         ]
                                         True
                checkEmuTestResult "BCD Add / Sub Test" tracefn h emures
            -- Add / Sub CVZN flag test
            do
                bin <- liftIO $ B.readFile "./tests/add_sub_cvzn_flag_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x01FF) (Left $ srFromString "--1B-I--")
                                         , CondLS (Addr 0x01FE) (Left $ srFromString "--1B-IZC")
                                         , CondLS (Addr 0x01FD) (Left $ srFromString "NV1B-I--")
                                         , CondLS (Addr 0x01FC) (Left $ srFromString "-V1B-I-C")
                                         , CondLS (Addr 0x01FB) (Left $ srFromString "--1B-I--")
                                         , CondLS (Addr 0x01FA) (Left $ srFromString "--1B-IZC")
                                         , CondLS (Addr 0x01F9) (Left $ srFromString "NV1B-I--")
                                         , CondLS (Addr 0x01F8) (Left $ srFromString "-V1B-I-C")
                                         , CondLS (Addr 0x01F7) (Left $ srFromString "N-1B-I--")
                                         , CondLS (Addr 0x01F6) (Left $ srFromString "-V1B-I-C")
                                         , CondLS (Addr 0x01F5) (Left $ srFromString "NV1B-I--")
                                         , CondLS A (Left 0x80)
                                         , CondLS SP (Left 0xF4)
                                         , CondCycleR 99 99
                                         ]
                                         True
                checkEmuTestResult "Add / Sub CVZN Flag Test" tracefn h emures
            -- CMP/BEQ/BNE test
            do
                bin <- liftIO $ B.readFile "./tests/cmp_beq_bne_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0015) (Left 0x7F)
                                         , CondLS A (Left 0x7F)
                                         , CondLS Y (Left 0x7F)
                                         , CondCycleR 152 152
                                         -- TODO: Test does not cover branch page crossing
                                         ]
                                         True
                checkEmuTestResult "CMP/BEQ/BNE Test" tracefn h emures
            -- CPX/CPY/BIT test
            do
                bin <- liftIO $ B.readFile "./tests/cpx_cpy_bit_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0042) (Left 0xA5)
                                         , CondLS A (Left 0xA5)
                                         , CondLS SR (Left $ srFromString "-V1--IZC")
                                         , CondCycleR 85 85
                                         ]
                                         True
                checkEmuTestResult "CPX/CPY/BIT Test" tracefn h emures
            -- Misc. branch test
            do
                bin <- liftIO $ B.readFile "./tests/misc_branch_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0080) (Left 0x1F)
                                         , CondLS A (Left 0x1F)
                                         , CondLS X (Left 0x0D)
                                         , CondLS Y (Left 0x54)
                                         , CondLS SR (Left $ srFromString "-V1--I-C-")
                                         , CondCycleR 108 108
                                         ]
                                         True
                checkEmuTestResult "Misc. Branch Test" tracefn h emures
            -- Flag test
            do
                bin <- liftIO $ B.readFile "./tests/flag_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0030) (Left 0xCE)
                                         , CondLS SR (Left $ srFromString "N-1--I--")
                                         , CondCycleR 29 29
                                         ]
                                         True
                checkEmuTestResult "Flag Test" tracefn h emures
            -- Special flag test
            do
                bin <- liftIO $ B.readFile "./tests/special_flag_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0020) (Left 0x3C)
                                         , CondLS (Addr 0x0021) (Left 0x6C)
                                         , CondLS SR (Left $ srFromString "--1-----")
                                         , CondLS SP (Left $ 0xFF)
                                         , CondCycleR 31 31
                                         ]
                                         True
                checkEmuTestResult "Special Flag Test" tracefn h emures
            -- Stack test
            do
                bin <- liftIO $ B.readFile "./tests/stack_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0030) (Left 0x29)
                                         , CondLS SR (Left $ srFromString "--1--I--")
                                         , CondLS SP (Left $ 0xFF)
                                         , CondCycleR 29 29
                                         ]
                                         True
                checkEmuTestResult "Stack Test" tracefn h emures
            -- RTI test
            do
                bin <- liftIO $ B.readFile "./tests/rti_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0033) (Left 0x42)
                                         , CondLS SR (Left $ srFromString "--1--I-C")
                                         , CondLS SP (Left $ 0xFF)
                                         , CondCycleR 40 40
                                         ]
                                         True
                checkEmuTestResult "RTI Test" tracefn h emures
            -- BRK test
            do
                bin <- liftIO $ B.readFile "./tests/brk_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC NOP
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x00FF) (Left 0x44)
                                         , CondLS SR (Left $ srFromString "--1--I--")
                                         , CondLS SP (Left $ 0xFF)
                                         , CondCycleR 89 89
                                         ]
                                         True
                checkEmuTestResult "BRK Test" tracefn h emures
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

