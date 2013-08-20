
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Instruction (Mnemonic(..), decodeInstruction, instructionLen)
import Emulator (runEmulator, Cond(..))
import MonadEmulator (LoadStore(..))
import Util (srFromString)

import Data.Monoid (All(..), getAll)
import Data.Word (Word8, Word64)
import Control.Monad (when, unless)
import Control.Monad.Writer (execWriterT, tell, WriterT)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import Data.Time (getZonedTime)
import System.IO (withFile, IOMode(..), hPutStrLn, Handle, hFlush, stdout)
import System.Mem (performGC)

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
    ([Cond], [Cond], [Cond], String, String, B.ByteString) ->
    WriterT All IO ()
checkEmuTestResult testName tracefn h ( condSuccess
                                      , condFailure
                                      , condStop
                                      , cpust
                                      , lastInst
                                      , trace
                                      ) = do
    let resultStr = (if null condFailure then "Succeeded" else "Failed") ++ ":\n" ++
                    "    Stop Reason      " ++ showCond condStop         ++ "\n"  ++
                    "    Unmet Conditions " ++ showCond condFailure      ++ "\n"  ++
                    "    Met Conditions   " ++ showCond condSuccess      ++ "\n"
        showCond []     = "[ ]"
        showCond (x:[]) = "[ " ++ show x ++ " ]"
        showCond (x:xs) = "[ " ++ show x ++ "\n"
                          ++ concatMap (\c -> "                     , " ++ show c ++ "\n") xs
                          ++                  "                     ]"
    liftIO $ do
        hPutStrLn h $ "--- " ++ testName ++ " ---\n"
        B.hPut    h trace
        hPutStrLn h ""
        hPutStrLn h resultStr
    unless (null condFailure) $ do
        tell $ All False
        liftIO $ do
            putStrLn $ testName ++ " " ++ resultStr ++
                "    CPU State        "             ++ cpust    ++ "\n" ++
                "    Last Instruction '"            ++ lastInst ++ "'\n" ++
                "    Trace            Written to '" ++ tracefn  ++ "'"
            hFlush stdout -- Show results immediately, don't wait for other tests

    -- After days of debugging an out-of-memory error, it became clear that the
    -- GC just isn't collecting. If I allocate some memory with an unboxed
    -- mutable vector inside the ST monad in the emulator (ring buffer trace
    -- log), the memory seemed to be retained sometimes. There's no reference to
    -- the vector outside of ST. Even if I never do anything but create the
    -- vector and put it inside the Reader record, and then only return a single
    -- Int from ST, which I immediately evaluate, the memory was retained
    -- (sometimes...). All memory profiles always showed I never allocate more
    -- than one vector at a time, yet multiple runs would cause OS memory for
    -- multiple vectors to be used and would eventually cause an out-of-memory
    -- error. Even then the RTS would not collect. Forcing collection after
    -- leaving ST and evaluating all return values seems to solve the problem
    -- entirely.
    liftIO performGC

runTests :: IO Bool
runTests = do
    let tracefn = "./trace.log"
    let traceMB = 64
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
                                         traceMB
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
                                         traceMB
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
                                         traceMB
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
                                         traceMB
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
                                         traceMB
                checkEmuTestResult "JMP/JSR/RTS Test" tracefn h emures
            -- JMP bug test
            do
                bin <- liftIO $ B.readFile "./tests/jump_bug_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0000) (Left 0x65)
                                         , CondLS PC (Right 0x061a)
                                         , CondCycleR 33 33
                                         ]
                                         True
                                         traceMB
                checkEmuTestResult "Jump Bug Test" tracefn h emures
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
                                         traceMB
                checkEmuTestResult "Register Transfer Test" tracefn h emures
            -- Add / Sub test
            do
                bin <- liftIO $ B.readFile "./tests/add_sub_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0030) (Left 0xAA)
                                         , CondLS SR (Left $ srFromString "N-1--I-C")
                                         , CondLS A (Left 0xAA)
                                         , CondLS X (Left 0x34)
                                         , CondLS Y (Left 0x01)
                                         , CondCycleR 205 205
                                         ]
                                         True
                                         traceMB
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
                                         traceMB
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
                                         , CondLS (Addr 0x01F4) (Left $ srFromString "NV1B-I--")
                                         , CondLS A (Left 0x80)
                                         , CondLS SP (Left 0xF3)
                                         , CondCycleR 108 108
                                         ]
                                         True
                                         traceMB
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
                                         traceMB
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
                                         traceMB
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
                                         , CondCycleR 109 109
                                         ]
                                         True
                                         traceMB
                checkEmuTestResult "Misc. Branch Test" tracefn h emures
            -- Branch backwards test
            do
                bin <- liftIO $ B.readFile "./tests/branch_backwards_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS X (Left 0xFF)
                                         , CondCycleR 31 31
                                         ]
                                         True
                                         traceMB
                checkEmuTestResult "Branch Backwards Test" tracefn h emures
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
                                         traceMB
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
                                         traceMB
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
                                         traceMB
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
                                         traceMB
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
                                         traceMB
                checkEmuTestResult "BRK Test" tracefn h emures
            -- KIL test
            do
                bin <- liftIO $ B.readFile "./tests/kil_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondLoopPC ]
                                         [ CondLS PC (Right $ 0x0600) ]
                                         True
                                         traceMB
                checkEmuTestResult "KIL Test" tracefn h emures
            -- Illegal NOP test
            do
                bin <- liftIO $ B.readFile "./tests/nop_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS PC (Right $ 0x0639)
                                         , CondCycleR 86 86
                                         ]
                                         True
                                         traceMB
                checkEmuTestResult "Illegal NOP Test" tracefn h emures
            -- LAX test
            do
                bin <- liftIO $ B.readFile "./tests/lax_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x01FF) $ Left 0x21
                                         , CondLS (Addr 0x01FE) $ Left 0x21
                                         , CondLS (Addr 0x01FD) $ Left 0xC3
                                         , CondLS (Addr 0x01FC) $ Left 0xC3
                                         , CondLS (Addr 0x01FB) $ Left 0x11
                                         , CondLS (Addr 0x01FA) $ Left 0x11
                                         , CondLS (Addr 0x01F9) $ Left 0xFF
                                         , CondLS (Addr 0x01F8) $ Left 0xFF
                                         , CondLS (Addr 0x01F7) $ Left 0x55
                                         , CondLS (Addr 0x01F6) $ Left 0x55
                                         , CondLS (Addr 0x01F5) $ Left 0xDB
                                         , CondLS (Addr 0x01F4) $ Left 0xDB
                                         , CondLS SR (Left $ srFromString "N-1--I--")
                                         , CondCycleR 135 135
                                         ]
                                         True
                                         traceMB
                checkEmuTestResult "LAX Test" tracefn h emures
            -- SAX test
            do
                bin <- liftIO $ B.readFile "./tests/sax_test.bin"
                let emures = runEmulator [ (bin, 0x0600) ]
                                         [ (PC, Right 0x0600) ]
                                         [ CondOpC BRK
                                         , CondCycleR 1000 (maxBound :: Word64)
                                         ]
                                         [ CondLS (Addr 0x0000) $ Left 0x80
                                         , CondLS (Addr 0x0001) $ Left 0x80
                                         , CondLS (Addr 0x0002) $ Left 0x80
                                         , CondLS (Addr 0x0003) $ Left 0x80
                                         , CondLS SR (Left $ srFromString "--1--I--")
                                         , CondCycleR 33 33
                                         ]
                                         True
                                         traceMB
                checkEmuTestResult "SAX Test" tracefn h emures
            -- NESTest CPU ROM test
            do
                bin <- liftIO $ B.readFile "./tests/nestest/nestest.bin"
                let emures = runEmulator [ (bin, 0x8000)
                                         , (bin, 0xC000)
                                         ]
                                         [ (PC, Right 0xC000)
                                         , (SP, Left 0xFD)
                                         ]
                                         [ CondLoopPC
                                         , CondCycleR 20000 (maxBound :: Word64) ]
                                         [ CondLS PC (Right 0x0000) 
                                         ]
                                         True
                                         traceMB
                checkEmuTestResult "NESTest CPU ROM Test" tracefn h emures
            {-
            -- Functional 6502 test
            do
                bin <- liftIO $ B.readFile "./tests/6502_functional_tests/6502_functional_test.bin"
                let emures = runEmulator [ (bin, 0x0400) ]
                                         [ (PC, Right 0x0400) ]
                                         [ CondLoopPC ]
                                         [ CondLS PC (Right 0x32E9) 
                                         , CondCycleR 92608051 92608051
                                         ]
                                         False
                                         traceMB
                checkEmuTestResult "Functional 6502 Test" tracefn h emures
            -}
        return $ getAll w

disassemble :: B.ByteString -> [B.ByteString]
disassemble bin = do
    let vec = VU.fromList $ B.unpack bin
        disassemble' pc =
            case decodeInstruction vec pc of
                Just instr -> let newPC   = pc + instructionLen instr
                                  showI   = B8.pack . show $ instr
                                  validPC = newPC < VU.length vec
                                  -- Build result using : instead of ++, no stack overflow etc.
                               in if validPC then showI : disassemble' newPC else [showI]
                Nothing -> []
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

