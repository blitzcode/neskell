
{-# LANGUAGE OverloadedStrings #-}

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
                                Left  w8  -> printf "SR == 0x%02X:%s" w8 (makeSRString w8)
                                _         -> error "Can't compare SR to 16 bit value"
    show (CondLS ls w   ) = case w of
                                Left  w8  -> printf "%s == 0x%02X" (show ls) w8
                                Right w16 -> printf "%s == 0x%04X" (show ls) w16
    show (CondOpC mn    ) = "OpCode(PC) == " ++ show mn
    show (CondCycleR l h) = unwords ["Cycle ∈ [", show l, ",", show h, "]"]
    show CondLoopPC       = "CondLoopPC"

checkCond :: MonadEmulator m => Cond -> m Bool
checkCond cond =
    case cond of
        CondLS     ls w -> case w of Left w8 -> (== w8) <$> load8 ls; Right w16 -> (== w16) <$> load16 ls
        CondOpC    mn   -> do (Instruction (OpCode decMn _) _) <- decodeInstructionM
                              return $ decMn == mn
        CondCycleR l h  -> do c <- getCycles
                              return $ (c >= l) && (c <= h)
        CondLoopPC      -> detectLoopOnPC =<< decodeInstructionM

loadBinary :: MonadEmulator m => B.ByteString -> Word16 -> m ()
loadBinary bin offs = do
    mapM_ (\i -> do let w8   = B.index bin i
                        addr = offs + fromIntegral i 
                    store8 (Addr addr) w8
                    -- TODO: This doesn't work if we start at an address not
                    --       divisible by 16
                    when (addr `mod` 16 == 0) . trace . printf "\n%04X" $ addr
                    trace $ printf " %02X" w8)
          [0..B.length bin - 1]

runEmulator ::
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
    , B.ByteString              -- Last traceMB MB of the execution trace
    )
runEmulator bins setup stopc verc traceEnable traceMB =
    runSTEmulator traceEnable traceMB $ do
        trace "Load Binary:\n"
        mapM_ (\(bin, offs) -> loadBinary bin offs) bins
        trace "\n\nSetup: "
        mapM_ (\(ls, w) ->
            case w of
                Left  w8  -> store8Trace  ls w8
                Right w16 -> store16Trace ls w16)
            $ (SP, Left 0xFF)
            : (SR, Left . setFlag FI . setFlag F1 $ 0)
            : setup
        trace "\n"
        let loop = do
                stop <- or <$> mapM (checkCond) stopc
                unless stop $ do
                    execute =<< decodeInstructionM
                    loop
         in do
                loop
                condSuccess <- filterM               (checkCond)   verc
                condFailure <- filterM (\x -> not <$> checkCond x) verc
                condStop    <- filterM               (checkCond)   stopc
                cpust       <- showCPUState
                cputrace    <- getTrace
                return (condSuccess, condFailure, condStop, cpust, cputrace)

