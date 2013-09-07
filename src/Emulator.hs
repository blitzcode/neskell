
{-# LANGUAGE OverloadedStrings #-}

module Emulator ( runEmulator
                , TraceMode(..)) where

-- Emulator main loop. Load a binary, set up the CPU and then run until a
-- termination criteria has been met

import Util
import MonadEmulator
import Execution
import Instruction
import Condition

import qualified Data.ByteString.Lazy as B
import Data.Word (Word8, Word16)
import Data.Monoid (getAll, All(..), mempty, (<>))
import Control.Monad (when, unless, filterM)
import Control.Applicative ((<$>))
import Text.Printf

traceMemory :: MonadEmulator m => Word16 -> Word16 -> m ()
traceMemory offs len =
    let go [] = return mempty
        go xs = do let (ls@(x:_), rest) = splitAt 16 xs
                   line <- mapM (load8 . Addr) ls
                   let zero = all (== 0) line
                   unless zero . trace $ printf "%04X" x ++ concatMap (printf " %02X") line ++ "\n"
                   (All zero <>) <$> go rest
     in do allzero <- getAll <$> go [offs..offs + len - 1]
           when allzero $ trace "(All Zero)\n"

loadBinary :: MonadEmulator m => B.ByteString -> Word16 -> m ()
loadBinary bin offs =
    mapM_ (\i -> let w8   = B.index bin i
                     addr = offs + fromIntegral i 
                  in store8 (Addr addr) w8)
          [0..B.length bin - 1]

data TraceMode = TraceNone | TraceBasic | TraceFullExe deriving Eq

runEmulator ::
    Processor                -> -- Model of the processor to be emulated
    [(B.ByteString, Word16)] -> -- List of program binaries and their offsets
    [(LoadStore, L8R16)]     -> -- Store operations to set up simulator state
    [Cond]                   -> -- The simulator will stop when any of these conditions are met
    [Cond]                   -> -- Success conditions to verify once stopped
    TraceMode                -> -- Level of tracing (TODO: Replace with trace-begin cond.?)
    Int                      -> -- MB of trace log ring buffer space
    ( [Cond]                    -- Success conditions which were met
    , [Cond]                    -- ...not met
    , [Cond]                    -- Stopping conditions met
    , String                    -- Debug string of last CPU state
    , String                    -- Instruction the PC is pointing at
    , B.ByteString              -- Last traceMB MB of the execution trace
    ) -- TODO: Use a type synonym or a record
runEmulator processor bins setup stopc verc trMode traceMB =
    runSTEmulator (trMode /= TraceNone) traceMB processor $ do
        trace "Load Binary:\n\n"
        mapM_ (\(bin, offs) -> do loadBinary bin offs
                                  traceMemory offs . fromIntegral . B.length $ bin) bins
        trace "\nSetup: "
        rvec <- load16 $ Addr 0xFFFC
        mapM_ (\(ls, w) ->
            case w of
                Left  w8  -> store8Trace  ls w8
                Right w16 -> store16Trace ls w16)
            $ (SP, Left 0xFF)
            : (SR, Left . setFlag FI . setFlag F1 $ 0)
            : (PC, Right rvec)
            : setup -- Run user setup code after the default one to allow overrides
        trace $
            "\n\n" ++
            "Cycles  PC   AC IX IY Status Reg. SP Instr. Operand ILnCycl Op. Load  Stores\n"    ++
            "Elapsed $PC  $A $X $Y $P:NV1BDIZC $S $I:Mne Data    [OIU]bC $Adr→$Val $Val→$Dst\n" ++
            "-------------------------------------------------------------------------------"
        -- Inlining everything (check, decode, execute) in this main loop makes a huge difference 
        let loop = do
                inst <- decodeInstructionM
                stop <- or <$> mapM (checkCond inst) stopc
                unless stop $ do
                    execute inst
                    loop
         in do
            if trMode /= TraceFullExe
                then trace "\n(Execution trace disabled)" >> runNoTrace loop
                else loop
            -- Detect Blargg's test ROMs, trace results
            magic <- mapM (load8 . Addr) [0x6001, 0x6002, 0x6003]
            when (magic == [0xDE, 0xB0, 0x61]) $ do
                trace "\n\nBlargg Test:\n"
                trace "    Memory at 0x6001 indicates we are running one of Blargg's test ROMs\n"
                trace . printf "    Result Code: 0x%02X\n" =<< (load8 $ Addr 0x6000)
                statusTxt <-     concatMap (++ "\\n") . lines
                             .   map (toEnum . fromIntegral :: Word8 -> Char)
                             .   takeWhile (/= 0)
                             <$> mapM (load8 . Addr) [0x6004..]
                trace $ "    Status Text: " ++ statusTxt
            -- Done, trace the first 512 bytes of memory and return other diagnostic information
            trace "\n\nZero Page:\n\n"
            traceMemory 0x0000 256
            trace "\nStack:\n\n"
            traceMemory 0x0100 256
            inst        <- decodeInstructionM
            condSuccess <- filterM               (checkCond inst  ) verc
            condFailure <- filterM (\x -> not <$> checkCond inst x) verc
            condStop    <- filterM               (checkCond inst  ) stopc
            cpust       <- showCPUState True
            cputrace    <- getTrace
            return ( condSuccess
                   , condFailure
                   , condStop
                   , cpust
                   , show inst
                   , cputrace
                   )

