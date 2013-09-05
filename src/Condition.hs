
{-# LANGUAGE ViewPatterns #-}

module Condition ( Cond(..)
                 , checkCond
                 , makeStackCond
                 , makeStringCond
                 ) where

-- Define conditions for emulator result verification / stopping

import Instruction (Mnemonic(..), Instruction(..), OpCodeView(..), viewOpCode)
import Util (L8R16, makeSRString)
import Execution (detectLoopOnPC)
import MonadEmulator (MonadEmulator(..), LoadStore(..))

import Control.Applicative ((<$>))
import Data.Word (Word8, Word16, Word64)
import Text.Printf
import Numeric (readHex)

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

-- Helpers for building more complex conditions

makeStackCond :: Word8 -> String -> [Cond]
makeStackCond initialSP stackstr =
    let val    = map (fst . head . readHex) . words $ stackstr
        spAddr = 0x0100 + fromIntegral initialSP
        addr   = [spAddr - fromIntegral (length val) + 1 .. spAddr]
     in zipWith (\v sp -> CondLS (Addr sp) (Left v)) val addr

makeStringCond :: Word16 -> String -> [Cond]
makeStringCond addr str =
    zipWith (\a c -> CondLS (Addr a) (Left . fromIntegral . fromEnum $ c)) [addr..] str

