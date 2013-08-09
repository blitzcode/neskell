
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

loadOperand8 :: MonadEmulator m => Instruction -> m Word8
loadOperand8 (Instruction (OpCode _ am) oper) =
    case oper of 
        []           -> case am of Accumulator -> load8 A
                                   _           -> err
        [w8]         -> case am of Immediate   -> return w8
                                   ZeroPage    -> load8 (Addr $ fromIntegral w8)
                                   ZeroPageX   -> do x <- load8 X
                                                     load8 (Addr . fromIntegral $ w8 + x)
                                   ZeroPageY   -> do y <- load8 X
                                                     load8 (Addr . fromIntegral $ w8 + y)
                                   Relative    -> return w8
                                   IdxInd      -> do x <- load8 X
                                                     let zp = w8 + x
                                                     l   <- load8 . Addr . fromIntegral $ zp
                                                     h   <- load8 . Addr . fromIntegral $ zp + 1
                                                     mem <- load8 . Addr $ makeW16 l h
                                                     return mem
                                   IndIdx      -> do l   <- load8 . Addr . fromIntegral $ w8
                                                     h   <- load8 . Addr . fromIntegral $ w8 + 1
                                                     y   <- load8 Y
                                                     mem <- load8 . Addr $ makeW16 l h + fromIntegral y
                                                     return mem
                                   _           -> err
        -- Absolute can either mean we want an address or the word at that
        -- location. Asking for an 8 bit operand means we'll return the word
        (opl:oph:[]) -> case am of Absolute  ->    load8 . Addr $ makeW16 opl oph
                                   AbsoluteX -> do x <- load8 X
                                                   load8 . Addr $ makeW16 opl oph + fromIntegral x
                                   AbsoluteY -> do y <- load8 Y
                                                   load8 . Addr $ makeW16 opl oph + fromIntegral y
                                   _         -> err
        _            -> err
  where
    err = trace "loadOperand8: OpLnErr" >> return 0

loadOperand16 :: MonadEmulator m => Instruction -> m Word16
loadOperand16 (Instruction (OpCode _ am) oper) =
    case oper of
        (opl:oph:[]) -> case am of
            Absolute  ->    return $ makeW16 opl oph
            AbsoluteX -> do x <- load8 X
                            return $ makeW16 opl oph + fromIntegral x
            AbsoluteY -> do y <- load8 Y
                            return $ makeW16 opl oph + fromIntegral y
            Indirect  -> do l <- load8 . Addr $ makeW16  opl      oph
                            -- The NMOS 6502 actually does this
                            h <- load8 . Addr $ makeW16 (opl + 1) oph
                            return $ makeW16 l h
            _         -> err
        _            -> err
  where
    err = trace "loadOperand16: OpLnErr" >> return 0

{-
loadOperand :: MonadEmulator m => Instruction -> m L8R16
loadOperand (Instruction (OpCode _ am) oper) =
    case am of
        Accumulator -> Left <$> load8 A
        Immediate   -> case oper of [w8] -> return $ Left w8
        ZeroPage    -> case oper of [w8] -> Left <$> load8 (Addr $ fromIntegral w8)
        ZeroPageX   -> case oper of [w8] -> do x <- load8 X
                                               Left <$> load8 (Addr . fromIntegral $ w8 + x)
        ZeroPageY   -> case oper of [w8] -> do y <- load8 X
                                               Left <$> load8 (Addr . fromIntegral $ w8 + y)
        Relative    -> case oper of [w8] -> return $ Left w8
        Absolute    -> case oper of (opl:oph:[]) -> return . Right $ makeW16 opl oph
        AbsoluteX   -> case oper of (opl:oph:[]) -> do x <- load8 X
                                                       return . Right $ makeW16 opl oph + fromIntegral x
        AbsoluteY   -> case oper of (opl:oph:[]) -> do y <- load8 Y
                                                       return . Right $ makeW16 opl oph + fromIntegral y
        Indirect    -> case oper of (opl:oph:[]) -> do l <- load8 . Addr $ makeW16  opl      oph
                                                       -- The NMOS 6502 actually does this
                                                       h <- load8 . Addr $ makeW16 (opl + 1) oph
                                                       return . Right $ makeW16 l h
        IdxInd      -> case oper of [w8] -> do x <- load8 X
                                               let zp = w8 + x
                                               l <- load8 . Addr . fromIntegral $ zp
                                               h <- load8 . Addr . fromIntegral $ zp + 1
                                               mem <- load8 . Addr $ makeW16 l h
                                               return $ Left mem
        IndIdx      -> case oper of [w8] -> do l <- load8 . Addr . fromIntegral $ w8
                                               h <- load8 . Addr . fromIntegral $ w8 + 1
                                               y <- load8 Y
                                               mem <- load8 . Addr $ makeW16 l h + fromIntegral y
                                               return $ Left mem
-}

execute :: MonadEmulator m => Instruction -> m ()
execute inst = do
    trace . B8.pack $ printf "\n%s (%ib): " (show inst) (instructionLen inst)
    updatePC ((fromIntegral $ instructionLen inst) +)
    case inst of
        Instruction (OpCode LDA am) oper -> return ()
        _ -> return ()
    cpustate <- showCPUState
    trace . B8.pack . printf "\n%s\n" $ cpustate

