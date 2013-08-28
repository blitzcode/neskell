
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Emulator ( runEmulator
                , Cond(..)
                ) where

-- Emulator main loop. Load a binary, set up the CPU and then run until a
-- termination criterea has been met

import Util
import MonadEmulator
import Execution
import Instruction

import qualified Data.ByteString.Lazy as B
import Data.Word (Word16, Word64)
import Data.Monoid (getAll, All(..), mempty, (<>))
import Control.Monad (when, unless, filterM)
import Control.Applicative ((<$>))
import Text.Printf

data Cond =
      CondLS     LoadStore L8R16 -- Compare any memory address / CPU state to
    | CondOpC    Mnemonic        -- PC pointing to a specific instruction type
    | CondCycleR Word64 Word64   -- Cycle count in the specified closed interval
    | CondLoopPC                 -- PC at an instruction jumping to its own address

instance Show Cond where
    show (CondLS SR w   ) = case w of
                                Left  w8  -> printf "SR == $%02X:%s" w8 (makeSRString w8)
                                _         -> error "Can't compare SR to 16 bit value"
    show (CondLS ls w   ) = case w of
                                Left  w8  -> printf "%s == $%02X" showLS w8
                                Right w16 -> printf "%s == $%04X" showLS w16
                              where
                                showLS = case ls of Addr _ -> "$"; _ -> ""; ++ show ls
    show (CondOpC mn    ) = "OpCode(PC) == " ++ show mn
    show (CondCycleR l h) = unwords ["Cycle ∈ [", show l, ",", show h, "]"]
    show CondLoopPC       = "CondLoopPC"

{-# INLINE checkCond #-}
-- Instruction is passed just to avoid decoding 2x
checkCond :: MonadEmulator m => Instruction -> Cond -> m Bool
checkCond inst@(Instruction (viewOpCode -> OpCode _ decMn _) _) cond =
    case cond of
        CondLS     ls w -> case w of Left w8 -> (== w8) <$> load8 ls; Right w16 -> (== w16) <$> load16 ls
        CondOpC    mn   -> return $ decMn == mn
        CondCycleR l h  -> do c <- getCycles
                              return $ (c >= l) && (c <= h)
        CondLoopPC      -> detectLoopOnPC inst

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

runEmulator ::
    Processor                -> -- Model of the processor to be emulated
    [(B.ByteString, Word16)] -> -- List of program binaries and their offsets
    [(LoadStore, L8R16)]     -> -- Store operations to set up simulator state
    [Cond]                   -> -- The simulator will stop when any of these conditions are met
    [Cond]                   -> -- Success conditions to verify once stopped
    Bool                     -> -- Enable execution tracing
    Int                      -> -- MB of trace log ring buffer space
    ( [Cond]                    -- Success conditions which were met
    , [Cond]                    -- ...not met
    , [Cond]                    -- Stopping conditions met
    , String                    -- Debug string of last CPU state
    , String                    -- Instruction the PC is pointing at
    , B.ByteString              -- Last traceMB MB of the execution trace
    )
runEmulator processor bins setup stopc verc traceEnable traceMB =
    runSTEmulator True traceMB processor $ do
        trace "Load Binary:\n\n"
        mapM_ (\(bin, offs) -> do loadBinary bin offs
                                  traceMemory offs . fromIntegral . B.length $ bin) bins
        trace "\nSetup: "
        mapM_ (\(ls, w) ->
            case w of
                Left  w8  -> store8Trace  ls w8
                Right w16 -> store16Trace ls w16)
            $ (SP, Left 0xFF)
            : (SR, Left . setFlag FI . setFlag F1 $ 0)
            : setup
        trace $ "\n\nCycles  PC   AC IX IY Status Reg. SP Instr. Operand ILnCycl Op. Load  Stores"    ++
                  "\nElapsed $PC  $A $X $Y $P:NV1BDIZC $S $I:Mne Data    [OIU]bC $Adr→$Val $Val→$Dst" ++
                "\n----------------------------------------------------------------------------------------"
        -- Inlining everything (check, decode, execute...) in this main loop makes a huge difference 
        let loop = do
                inst <- decodeInstructionM
                stop <- or <$> mapM (checkCond inst) stopc
                unless stop $ do
                    execute inst
                    loop
         in do
                if traceEnable
                    then loop
                    else trace "\n(Execution trace disabled)" >> runNoTrace loop
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

