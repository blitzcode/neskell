
{-# LANGUAGE OverloadedStrings #-}

module Execution ( execute
                 ) where

-- The actual emulation of all 6502 instructions running inside of MonadEmulator

import MonadEmulator
import Instruction
import Util

import Data.Word (Word8, Word16)
import qualified Data.ByteString.Char8 as B8
import Text.Printf
import Control.Applicative ((<$>))

-- data AddressMode = Implied | Accumulator | Immediate | ZeroPage | ZeroPageX | ZeroPageY | Relative | Absolute | AbsoluteX | AbsoluteY | Indirect | IdxInd | IndIdx
-- data OpCode = OpCode Mnemonic AddressMode
-- data Instruction = Instruction OpCode [Word8]
-- data LoadStore = A | X | Y | SR | SP | PC | PCL | PCH | Addr Word16
-- class (Functor m, Monad m) => MonadEmulator m where
--    load8   :: LoadStore -> m Word8
--    load16  :: LoadStore -> m Word16
--    store8  :: LoadStore -> Word8  -> m ()
--    store16 :: LoadStore -> Word16 -> m ()

-- Functions for loading and storing 8 bit operands for any instruction.
-- Illegal instructions (writing to an Immediate operand, reading using the
-- Indirect mode, having no operand data for anything but
-- Immediate/Accumulator, etc.) will result in an error trace and a dummy
-- return value

getOperandAddr8 :: MonadEmulator m => Instruction -> m LoadStore
getOperandAddr8 inst@(Instruction (OpCode _ am) oper) =
    case oper of 
        []           -> case am of Accumulator -> return A
                                   _           -> err
        [w8]         -> case am of ZeroPage    -> return . Addr $ fromIntegral w8
                                   ZeroPageX   -> do x <- load8 X
                                                     return $ Addr (fromIntegral $ w8 + x)
                                   ZeroPageY   -> do y <- load8 Y
                                                     return $ Addr (fromIntegral $ w8 + y)
                                   IdxInd      -> do x <- load8 X
                                                     let zp = w8 + x
                                                     l <- load8 . Addr . fromIntegral $ zp
                                                     h <- load8 . Addr . fromIntegral $ zp + 1
                                                     return . Addr $ makeW16 l h
                                   IndIdx      -> do l <- load8 . Addr . fromIntegral $ w8
                                                     h <- load8 . Addr . fromIntegral $ w8 + 1
                                                     y <- load8 Y
                                                     return . Addr $ makeW16 l h + fromIntegral y
                                   _           -> err
        (opl:oph:[]) -> case am of Absolute  ->    return . Addr $ makeW16 opl oph
                                   AbsoluteX -> do x <- load8 X
                                                   return . Addr $ makeW16 opl oph + fromIntegral x
                                   AbsoluteY -> do y <- load8 Y
                                                   return . Addr $ makeW16 opl oph + fromIntegral y
                                   _         -> err
        _            -> err
  where
    err = trace (B8.pack $ "getOperandAddr8: AM/OpLen Error: " ++ show inst) >> return A

loadOperand8 :: MonadEmulator m => Instruction -> m Word8
loadOperand8 inst@(Instruction (OpCode _ am) oper) =
    case oper of 
        [w8] -> case am of Immediate -> return w8
                           _         -> load8 =<< getOperandAddr8 inst
        _    ->                         load8 =<< getOperandAddr8 inst

storeOperand8 :: MonadEmulator m => Instruction -> Word8 -> m ()
storeOperand8 inst val = (\ls -> store8 ls val) =<< getOperandAddr8 inst

-- There are no instructions storing 16 bit operands, and the only instructions
-- that load them for actually doing anything with them besides looking up an 8
-- bit value (covered by loadOperand8) are JMP / JSR with Absolute / Indirect
-- addressing

loadOperand16 :: MonadEmulator m => Instruction -> m Word16
loadOperand16 inst@(Instruction (OpCode _ am) oper) =
    case oper of
        (opl:oph:[]) -> case am of
            Absolute  ->    return $ makeW16 opl oph
            Indirect  -> do l <- load8 . Addr $ makeW16  opl      oph
                            -- The NMOS 6502 actually does this
                            h <- load8 . Addr $ makeW16 (opl + 1) oph
                            return $ makeW16 l h
            _         -> err
        _            -> err
  where
    err = trace (B8.pack $ "loadOperand16: AM/OpLen Error: " ++ show inst) >> return 0

updatePC :: MonadEmulator m => (Word16 -> Word16) -> m ()
updatePC f = load16 PC >>= return . f >>= store16 PC

execute :: MonadEmulator m => Instruction -> m ()
execute inst@(Instruction (OpCode mn _) _) = do
    trace . B8.pack $ printf "\n%s (%ib): " (show inst) (instructionLen inst)
    updatePC ((fromIntegral $ instructionLen inst) +)
    advCycles 3
    case mn of
        LDA -> do
            a <- loadOperand8 inst
            store8 A a
        LDX -> do
            x <- loadOperand8 inst
            store8 X x
        LDY -> do
            y <- loadOperand8 inst
            store8 Y y
        STA -> do
            a <- load8 A
            storeOperand8 inst a
        STX -> do
            x <- load8 X
            storeOperand8 inst x
        STY -> do
            y <- load8 Y
            storeOperand8 inst y
        _ -> return ()
    cpustate <- showCPUState
    trace . B8.pack . printf "\n%s\n" $ cpustate

