
{-# LANGUAGE OverloadedStrings, FlexibleContexts, LambdaCase #-}

module Test (runTests) where

import Instruction (Mnemonic(..), disassemble)
import Emulator (runEmulator, Cond(..))
import MonadEmulator (LoadStore(..), Processor(..))
import Util (srFromString)

import Data.Monoid (All(..), getAll)
import Data.Word (Word8, Word16, Word64)
import Control.Monad (when, unless, guard, void)
import Control.Monad.Writer (execWriterT, tell)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Time (getZonedTime)
import System.IO (withFile, IOMode(..), hPutStrLn, Handle, hFlush, stdout)
import System.Mem (performGC)
import Numeric (readHex)
import Control.Concurrent.Async (Async, async, wait)
import Control.Applicative ((<$>), (<*>))
import GHC.Conc (getNumProcessors)
import Control.Concurrent (getNumCapabilities, setNumCapabilities)
--import Control.Concurrent.STM.TSem
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad.State (execStateT, get, put, modify)
--     Control.Monad.State.Strict
import qualified System.Console.ANSI as A
import Text.Printf
import qualified Control.Concurrent.SSem as SS

-- TODO: Try running the listen thread not in the main thread (see parallel
--       Haskell book)
-- TODO: Run queue and sempahore operations in the same transaction, see if that
--       helps
-- TODO: Profile with ThreadScope
-- TODO: Have listen thread print out number of running tests every 100ms
-- TODO: Read TQueue documentation, compare with other channel variants
-- TODO: If semaphore + Async impl. can't be fixed, consider just using a forkIO
--       per core and read test tasks from a queue
-- TODO: Investigate if irregular runtime for the quick test suite is due to the
--       file IO or the ring buffer tracing itself
-- TODO: The GC bug is back, and this time it seems performGC can only lessen
--       its impact, not fix it
-- TODO: Double check if any of the memory / performance issues happen when
--       using only a single OS thread / no -threaded
-- TODO: Use traceEventIO & labelThread functions and enable the event log,
--       analyze behavior of threads, why are there stalls and such large run
--       time variations? (ghc-events tool, build -eventlog & run +RTS -l)
-- TODO: Play around with GC & Thread RTS options (see GHC docs and parallel
--       Haskell book)

data TestEvent = TestRunning | TestSucceess | TestFailure String String
data TestStatus = TestStatus { tsRunning   :: Int
                             , tsSucceeded :: Int
                             , tsFailed    :: Int
                             , tsFailures  :: [String]
                             }

runTests :: Bool -> IO Bool
runTests onlyQuickTests = do
    cores      <- getNumProcessors
    osThreads  <- getNumCapabilities
    eventQueue <- newTQueueIO :: IO (TQueue (Int, TestEvent))
    --sem        <- atomically $ newTSem osThreads
    sem        <- SS.new osThreads
    -- Run testsuite
    tests      <- testSuite onlyQuickTests eventQueue sem
    let numTests = length tests
    -- Header and initial all-pending status bar
    void $ printf "\nChecking %2i test cases on %2i core(s) with %2i thread(s)\n"
        numTests cores osThreads
    putStr "------------------------------------------------------\n\n"
    putStr $ "[" ++ replicate numTests '·' ++ "] 0%\n\n\n\n"
    -- Process events from test threads
    let status = TestStatus { tsRunning   = 0
                            , tsSucceeded = 0
                            , tsFailed    = 0
                            , tsFailures  = []
                            }
    overallRes <-
        let processEvents ts = do
            (testID, e) <- atomically $ readTQueue eventQueue
            let invColored color = A.setSGRCode [ A.SetSwapForegroundBackground True
                                                , A.SetColor A.Foreground A.Vivid color
                                                ]
            -- Update test status and status bar
            A.cursorUp $ 4 + (length $ tsFailures ts)
            A.setCursorColumn $ 1 + testID
            ts' <- case e of
                TestRunning        -> do putStr $ invColored A.Yellow ++ "R"
                                         return ts { tsRunning = tsRunning ts + 1 }
                TestSucceess       -> do putStr $ invColored A.Green  ++ "S"
                                         return ts { tsRunning   = tsRunning   ts - 1
                                                   , tsSucceeded = tsSucceeded ts + 1
                                                   }
                TestFailure tn tfn -> do putStr $ invColored A.Red    ++ "F"
                                         return ts { tsRunning  = tsRunning ts - 1
                                                   , tsFailed   = tsFailed  ts + 1
                                                   , tsFailures =
                                                         printf "%s%s Failed, see %s%s\n"
                                                            (invColored A.Red) tn tfn (A.setSGRCode [])
                                                         : tsFailures ts
                                                   }
            A.setSGR []
            A.setCursorColumn $ 3 + numTests
            A.clearFromCursorToLineEnd
            void $ printf "%.2f%%"
                (fromIntegral (tsSucceeded ts' + tsFailed ts') /
                 fromIntegral numTests * 100.0 :: Float)
            A.cursorDown 2
            A.setCursorColumn 0
            -- Summary
            A.clearLine
            void $ printf "Running: %s%2i%s | Succeeded: %s%2i%s | Failed: %s%2i%s | Pending: %i\n\n"
                (invColored A.Yellow) (tsRunning   ts') (A.setSGRCode [])
                (invColored A.Green ) (tsSucceeded ts') (A.setSGRCode [])
                (invColored A.Red   ) (tsFailed    ts') (A.setSGRCode [])
                (numTests - (tsRunning ts' + tsSucceeded ts' + tsFailed ts'))
            -- Failures
            if null $ tsFailures ts'
                then printf "%s(No Failures)%s" (invColored A.Green) (A.setSGRCode [])
                else mapM_ (\x -> putStr $ A.clearLineCode ++ x) $ tsFailures ts'
            -- Done?
            hFlush stdout
            if (tsSucceeded ts' + tsFailed ts' < numTests)
                then processEvents ts'
                else putStr "\n\n" >> (return $ tsFailed ts' == 0)
         in processEvents status
    mapM_ wait tests
    return overallRes

-- Trace emulator run results into log file, return success / failure of test
checkEmuTestResult ::
    String ->
    String ->
    ([Cond], [Cond], [Cond], String, String, B.ByteString) ->
    IO Bool
checkEmuTestResult testName logFile ( condSuccess
                                    , condFailure
                                    , condStop
                                    , cpust
                                    , nextInst
                                    , trace
                                    ) = do
    let resultStr = (if null condFailure then "Succeeded" else "Failed") ++ ":\n" ++
                    "    Stop Reason      " ++ showCond condStop         ++  "\n" ++
                    "    Unmet Conditions " ++ showCond condFailure      ++  "\n" ++
                    "    Met Conditions   " ++ showCond condSuccess      ++  "\n" ++
                    "    CPU State        " ++ cpust                     ++  "\n" ++
                    "    Next Instruction " ++ nextInst                  ++  "\n"
        showCond []     = "[ ]"
        showCond (x:[]) = "[ " ++ show x ++ " ]"
        showCond (x:xs) = "[ " ++ show x ++ "\n"
                          ++ concatMap (\c -> "                     , " ++ show c ++ "\n") xs
                          ++                  "                     ]"
    liftIO $ withFile logFile WriteMode $ \h -> do
        time <- getZonedTime
        hPutStrLn h $ "Trace Log " ++ show time ++ "\n"
        hPutStrLn h $ "--- " ++ testName ++ " ---\n"
        B.hPut    h trace
        hPutStrLn h ""
        hPutStrLn h resultStr
    return $ null condFailure

-- Helpers for building emulator success / stop conditions
makeStackCond :: Word8 -> String -> [Cond]
makeStackCond initialSP stackstr =
    let val    = map (fst . head . readHex) . words $ stackstr
        spAddr = 0x0100 + fromIntegral initialSP
        addr   = [spAddr - fromIntegral (length val) + 1 .. spAddr]
     in zipWith (\v sp -> CondLS (Addr sp) (Left v)) val addr
makeStringCond :: Word16 -> String -> [Cond]
makeStringCond addr str =
    zipWith (\a c -> CondLS (Addr a) (Left . fromIntegral . fromEnum $ c)) [addr..] str

--withSemaphore :: TSem -> IO () -> IO ()
--withSemaphore sem f = (atomically $ waitTSem sem) >> f >> (atomically $ signalTSem sem)

binToLogFn :: String -> String
binToLogFn fn = takeWhile (/= '.') fn ++ ".log"

-- Run the testsuite, communicate completion and success / failure status
-- through the queue. The TSem is used to limit concurrency
testSuite :: Bool -> TQueue (Int, TestEvent) -> {-TSem-} SS.SSem -> IO [(Async ())]
testSuite onlyQuickTests eventQueue sem = do
    -- All later tests are actual emulator tests, but start with the decoding test
    do
        bin <- B.readFile "./tests/decoding/instr_test.bin"
        ref <- B.readFile "./tests/decoding/instr_test_ref_disasm.asm"
        case decodingTest bin ref of
            Left err -> putStrLn $ "Decoding Test Failed: " ++ err
            Right _  -> return ()
    -- We use a State monad to accumulate the Asyncs while the Maybe is for
    -- early exit in case we only want to run a subset of the test suite
    let traceMB   = 64
        tracePath = "./trace/"
    flip execStateT [] . runMaybeT $ do -- MaybeT (StateT [(Async ())]) ()
        let runTestAsync testName logFile test = do
            s <- get
            -- Limit concurrency through semaphore (memory consumption for
            -- tracing would otherwise explode)
            t <- liftIO . async . {-withSemaphore-}SS.withSem sem $ do
                let testID = length s -- Just a unique ID based on the list position
                atomically $ writeTQueue eventQueue (testID, TestRunning)
                success <- checkEmuTestResult testName logFile =<< test
                atomically $ writeTQueue eventQueue
                    ( testID
                    ,   if success
                      then TestSucceess
                      else TestFailure testName logFile
                    )
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
                -- entirely. This seems like a bug in the GC, running out of OS memory
                -- instead of garbage collecting allocations it (demonstrably) knows how to
                -- free, while all RTS memory profiles confirm it is indeed not referenced.
                -- This is also not related to any concurrency, happened already in the
                -- single threaded version.
                liftIO performGC
            put $ t : s -- Put Async into State

        -- Tests follow

        runTestAsync "Load / Store Test" (tracePath ++ "load_store_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/load_store_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "AND / OR / XOR Test" (tracePath ++ "and_or_xor_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/and_or_xor_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "INC / DEC Test" (tracePath ++ "inc_dec_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/inc_dec_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Bitshift Test" (tracePath ++ "bitshift_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/bitshift_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "JMP/JSR/RTS Test" (tracePath ++ "jump_ret_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/jump_ret_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Jump Bug Test" (tracePath ++ "jump_bug_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/jump_bug_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Register Transfer Test" (tracePath ++ "reg_transf_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/reg_transf_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Add / Sub Test" (tracePath ++ "add_sub_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/add_sub_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "CMP/BEQ/BNE Test" (tracePath ++ "cmp_beq_bne_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/cmp_beq_bne_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  [ CondLS (Addr 0x0015) (Left 0x7F)
                                  , CondLS A (Left 0x7F)
                                  , CondLS Y (Left 0x7F)
                                  , CondCycleR 152 152
                                  ]
                                  True
                                  traceMB

        runTestAsync "CPX/CPY/BIT Test" (tracePath ++ "cpx_cpy_bit_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/cpx_cpy_bit_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Misc. Branch Test" (tracePath ++ "misc_branch_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/misc_branch_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Branch Backwards Test" (tracePath ++ "branch_backwards_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/branch_backwards_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  [ CondLS X (Left 0xFF)
                                  , CondCycleR 31 31
                                  ]
                                  True
                                  traceMB

        runTestAsync "Branch Pagecrossing Test" (tracePath ++ "branch_pagecross_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/branch_pagecross_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x02F9) ]
                                  [ (PC, Right 0x02F9) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  [ CondLS A (Left 0xFF)
                                  , CondCycleR 14 14
                                  ]
                                  True
                                  traceMB

        runTestAsync "Flag Test" (tracePath ++ "flag_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/flag_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Special Flag Test" (tracePath ++ "special_flag_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/special_flag_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Stack Test" (tracePath ++ "stack_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/stack_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "BCD Add / Sub Test" (tracePath ++ "bcd_add_sub_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/bcd_add_sub_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP (Left 0xF6)
                                    , CondLS SR (Left $ srFromString "N-1-DI--")
                                    , CondCycleR 73 73
                                    ]
                                    ++ makeStackCond 0xFF "87 91 29 27 34 73 41 46 05"
                                  )
                                  True
                                  traceMB

        runTestAsync "Add / Sub CVZN Flag Test" (tracePath ++ "add_sub_cvzn_flag_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/add_sub_cvzn_flag_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS A (Left 0x80)
                                    , CondLS SP (Left 0xF3)
                                    , CondCycleR 108 108
                                    ]
                                    ++ zipWith (\sr sp -> CondLS (Addr sp) (Left $ srFromString sr))
                                               [ "--1B-I--"
                                               , "--1B-IZC"
                                               , "NV1B-I--"
                                               , "-V1B-I-C"
                                               , "--1B-I--"
                                               , "--1B-IZC"
                                               , "NV1B-I--"
                                               , "-V1B-I-C"
                                               , "N-1B-I--"
                                               , "-V1B-I-C"
                                               , "NV1B-I--"
                                               , "NV1B-I--"
                                               ]
                                               (reverse [0x01F4..0x01FF])
                                  )
                                  True
                                  traceMB

        runTestAsync "RTI Test" (tracePath ++ "rti_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/hmc-6502/rti_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "BRK Test" (tracePath ++ "brk_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/brk_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "KIL Test" (tracePath ++ "kil_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/kil_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondLoopPC ]
                                  [ CondLS PC (Right $ 0x0600) ]
                                  True
                                  traceMB

        runTestAsync "Illegal NOP Test" (tracePath ++ "nop_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/nop_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  [ CondLS PC (Right $ 0x0639)
                                  , CondCycleR 86 86
                                  ]
                                  True
                                  traceMB

        runTestAsync "LAX Test" (tracePath ++ "lax_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/lax_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP (Left 0xF3)
                                    , CondLS SR (Left $ srFromString "N-1--I--")
                                    , CondCycleR 135 135
                                    ]
                                    ++ makeStackCond 0xFF "DB DB 55 55 FF FF 11 11 C3 C3 21 21"
                                  )
                                  True
                                  traceMB

        runTestAsync "SAX Test" (tracePath ++ "sax_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/sax_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
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

        runTestAsync "Illegal RMW Test" (tracePath ++ "illegal_rmw_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/illegal_rmw_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600)
                                  ]
                                  [ CondOpC BRK
                                  , CondCycleR 3000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP $ Left 0x81
                                    , CondCycleR 1158 1158
                                    ]
                                    ++ ( makeStackCond 0xFF $
                                             "      00 34 0F 4C B5 D5 4E 35 7D 7B B4 8D 04 35 " ++
                                             "0D 50 B4 A0 76 B5 9B 00 B5 BF 32 B5 BB 3A 35 3B " ++
                                             "EC B5 FE 22 B5 B3 42 B5 F2 BA B5 FF 80 B5 80 CC " ++
                                             "75 66 EF 35 7B 78 35 6A 9D B4 D7 79 35 59 11 34 " ++
                                             "35 01 34 01 33 35 11 3B 35 33 ED B5 E4 21 37 00 " ++
                                             "42 35 40 43 34 01 00 B5 FE 9A B4 FE 35 B4 97 F1 " ++
                                             "B4 FE 3B B4 FE F3 B4 FC 38 B4 FF FF 37 FF 98 35 " ++
                                             "99 33 37 33 EF 35 F0 39 35 3A F1 B4 F0 36 35 37 "
                                       )
                                  )
                                  True
                                  traceMB

        runTestAsync "Illegal XB Test" (tracePath ++ "illegal_xb_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/illegal_xb_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP $ Left 0xDE
                                    -- TODO: Cycle count from Visual 6502,
                                    --       also check against VICE
                                    , CondCycleR 267 267
                                    ]
                                    ++ ( makeStackCond 0xFF $
                                             "                                             FF " ++
                                             "B4 4C 35 A0 A0 A0 B4 80 B5 FF 36 00 75 55 F5 D5 " ++
                                             "35 7F 37 00 35 40 B5 CD 36 00 B5 AA 34 01 36 00 "
                                       )
                                  )
                                  True
                                  traceMB

        runTestAsync "Illegal BCD Test" (tracePath ++ "illegal_bcd_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/illegal_bcd_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP $ Left 0xDD
                                    , CondCycleR 210 210
                                    ]
                                    ++ ( makeStackCond 0xFF $
                                             "                                          9A BD " ++
                                             "9A BD 0A 3D 0A 3D 99 BC 00 3F 99 BC 76 3C 74 3C " ++
                                             "E0 BD D0 7D 66 3F 65 3D 75 7D 80 FC 80 FC 00 3E "
                                       )
                                  )
                                  True
                                  traceMB

        runTestAsync "ARR BCD Test" (tracePath ++ "arr_bcd_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/arr_bcd_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP $ Left 0xE7
                                    , CondCycleR 150 150
                                    ]
                                    ++ ( makeStackCond 0xFF $
                                             "                        3C 0D 3D D8 3D D5 7D 80 " ++
                                             "3E 00 3D D5 BC 8D BD 58 BD 55 FD 00 BC 80 BD 55 "
                                       )
                                  )
                                  True
                                  traceMB

        runTestAsync "AHX/TAS/SHX/SHY Test" (tracePath ++ "ahx_tas_shx_shy_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/ahx_tas_shx_shy_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP $ Left 0xF2
                                    , CondCycleR 232 232
                                    ]
                                    ++ makeStackCond 0xFF "01 C9 01 80 C0 E0 01 55 80 80 01 34 10"
                                  )
                                  True
                                  traceMB

        runTestAsync "AHX/TAS/SHX/SHY Pagecrossing Test"
          (tracePath ++ "ahx_tas_shx_shy_pagecross_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/ahx_tas_shx_shy_pagecross_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK
                                  , CondCycleR 1000 (maxBound :: Word64)
                                  ]
                                  ( [ CondLS SP $ Left 0xF5
                                    , CondCycleR 212 212
                                    ]
                                    ++ makeStackCond 0xFF "42 42 41 41 00 01 CE CF C0 D0"
                                  )
                                  True
                                  traceMB

        runTestAsync "NESTest CPU ROM Test" (tracePath ++ "nestest.log") $ do
            bin <- liftIO $ B.readFile "./tests/nestest/nestest.bin"
            return $! runEmulator NES_2A03
                                  [ (bin, 0xC000) ]
                                  [ (PC, Right 0xC000)
                                  , (SP, Left 0xFD)
                                  ]
                                  [ CondLS PC (Right 0x0001)
                                  , CondCycleR 30000 (maxBound :: Word64)
                                  ]
                                  [ CondLS PC (Right 0x0001)
                                  , CondLS (Addr 0x0002) $ Left 0x00
                                  , CondLS (Addr 0x0003) $ Left 0x00
                                  , CondLS A  $ Left 0x00
                                  , CondLS X  $ Left 0xFF
                                  , CondLS Y  $ Left 0x15
                                  , CondLS SP $ Left 0xFF
                                  , CondLS SR (Left $ srFromString "--1--IZC")
                                  -- TODO: Verify cycles against Visual 2A03. Not sure if that's
                                  --       possible, though. IIRC it has a full 6502 with BCD,
                                  --       and this code does not work correctly if the BCD flag
                                  --       is obeyed
                                  , CondCycleR 26553 26553
                                  ]
                                  True
                                  traceMB

        -- Tests below here take a long time to run. We try to keep the
        -- quick test suite under one second, this will run for minutes
        guard $ not onlyQuickTests

        mapM_ (\(file, cycles) ->
            runTestAsync ("Blargg's " ++ file ++ " Test") (tracePath ++ binToLogFn file) $ do
                bin <- liftIO . B.readFile $ "./tests/instr_test-v4/rom_singles/" ++ file
                return $! runEmulator NES_2A03
                                      [ (bin, 0x8000) ]
                                      [ (SP, Left 0xFD)
                                        -- Don't accidentally trigger stop condition right at startup
                                      , (Addr 0x6000, Left 0xFF)
                                      ]
                                      [ -- The test will eventually do an endless 'BEQ $FE' loop,
                                        -- but we can abort as soon as the result code is written
                                        -- and skip a lot of cycles used for audio/video result
                                        -- reporting
                                        --
                                        -- TODO: This only aborts early on success...
                                        CondLS (Addr 0x6000) $ Left 0x00
                                      , CondCycleR (cycles * 2) (maxBound :: Word64)
                                      ]
                                      ( [ CondLS (Addr 0x6000) $ Left 0x00 -- Success
                                        , CondLS (Addr 0x6001) $ Left 0xDE -- Magic
                                        , CondLS (Addr 0x6002) $ Left 0xB0
                                        , CondLS (Addr 0x6003) $ Left 0x61
                                        , CondCycleR cycles cycles 
                                        ] ++
                                        makeStringCond
                                             0x6004
                                             ("\n" ++ takeWhile (/= '.') file ++ "\n\nPassed\n")
                                     )
                                     False
                                     traceMB)
            -- TODO: Verify those cycle counts against a reference
            [ ( "01-basics.bin"   , 330200   )
            , ( "02-implied.bin"  , 2687506  )
            , ( "03-immediate.bin", 2388550  )
            , ( "04-zero_page.bin", 3273464  )
            , ( "05-zp_xy.bin"    , 7558100  )
            , ( "06-absolute.bin" , 3093993  )
            , ( "07-abs_xy.bin"   , 10675054 )
            , ( "08-ind_x.bin"    , 4145448  )
            , ( "09-ind_y.bin"    , 3888212  )
            , ( "10-branches.bin" , 1033363  )
            , ( "11-stack.bin"    , 4682762  )
            , ( "12-jmp_jsr.bin"  , 322698   )
            , ( "13-rts.bin"      , 223119   )
            , ( "14-rti.bin"      , 225084   )
            , ( "15-brk.bin"      , 540320   )
            , ( "16-special.bin"  , 157793   )
            ]
        -- TODO: Add more tests from Blargg's test suite

        runTestAsync "Functional 6502 Test" (tracePath ++ "6502_functional_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/6502_functional_tests/6502_functional_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0400) ]
                                  [ (PC, Right 0x0400) ]
                                  [ CondLoopPC ]
                                  [ CondLS PC (Right 0x32E9)
                                  -- TODO: Verify cycle count
                                  , CondCycleR 92608051 92608051
                                  ]
                                  False
                                  traceMB

        runTestAsync "Full BCD Test" (tracePath ++ "full_bcd_test.log") $ do
            bin <- liftIO $ B.readFile "./tests/unit/full_bcd_test.bin"
            return $! runEmulator NMOS_6502
                                  [ (bin, 0x0600) ]
                                  [ (PC, Right 0x0600) ]
                                  [ CondOpC BRK ]
                                  [ CondLS (Addr 0x0600) $ Left 0x00
                                  , CondCycleR 61821255 61821255
                                  ]
                                  False
                                  traceMB

decodingTest :: B.ByteString -> B.ByteString -> Either String ()
decodingTest bin ref = do
    let dasm = disassemble bin
        refl = B8.lines ref
        loop r d = do
            let mkSafe = \case [] -> ""; (x:_) -> x
                safeR  = mkSafe r
                safeD  = mkSafe d
            when (safeR /= safeD) . throwError $
                "Expected '" ++ B8.unpack safeR ++ "' got '" ++ B8.unpack safeD ++ "'"
            unless (null r || null d) $ loop (tail r) (tail d)
     in loop refl dasm

