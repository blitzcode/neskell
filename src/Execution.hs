
{-# LANGUAGE OverloadedStrings #-}

module Execution ( execute
                 ) where

-- The actual emulation of all 6502 instructions running inside of MonadEmulator

import MonadEmulator
import Instruction
import Util

import Data.Word (Word8, Word16, Word64)
import qualified Data.ByteString.Char8 as B8
import Text.Printf
import Data.Bits (testBit, (.&.), (.|.), xor, shiftL, shiftR)
import Control.Applicative ((<$>))

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
                           Relative  -> return w8
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
                            -- The NMOS 6502 actually does it like this
                            h <- load8 . Addr $ makeW16 (opl + 1) oph
                            return $ makeW16 l h
            _         -> err
        _            -> err
  where
    err = trace (B8.pack $ "loadOperand16: AM/OpLen Error: " ++ show inst) >> return 0

update16 :: MonadEmulator m => LoadStore -> (Word16 -> Word16) -> m ()
update16 ls f = load16 ls >>= return . f >>= store16 ls

setNZ :: Word8 -> Word8 -> Word8
setNZ x sr =
    let isN = testBit x 7
        isZ = x == 0
     in modifyFlag FN isN . modifyFlag FZ isZ $ sr

updateNZ :: MonadEmulator m => Word8 -> m ()
updateNZ x = do
    sr <- load8 SR
    store8 SR $ setNZ x sr

updateNZC :: MonadEmulator m => Word8 -> Bool -> m ()
updateNZC x carry = do
    sr <- load8 SR
    store8 SR . setNZ x . modifyFlag FC carry $ sr

storeStack8 :: MonadEmulator m => Word8 -> m ()
storeStack8 w8 = do
    sp <- load8 SP
    store8 (Addr $ 0x0100 + fromIntegral sp) w8
    store8 SP (sp - 1)

storeStack16 :: MonadEmulator m => Word16 -> m ()
storeStack16 w16 = do
    let (l, h) = splitW16 w16
    sp <- load8 SP
    let sp1 = sp - 1
    let sp2 = sp - 2
    store8 (Addr $ 0x0100 + fromIntegral sp1) h
    store8 (Addr $ 0x0100 + fromIntegral sp2) l
    store8 SP (sp - 2)

loadStack8 :: MonadEmulator m => m Word8
loadStack8 = do
    sp <- (+) 1 <$> load8 SP
    w8 <- load8 (Addr $ 0x0100 + fromIntegral sp )
    store8 SP sp
    return w8

loadStack16 :: MonadEmulator m => m Word16
loadStack16 = do
    sp <- load8 SP
    let sp1 = sp + 1
    l <- load8 (Addr $ 0x0100 + fromIntegral sp )
    h <- load8 (Addr $ 0x0100 + fromIntegral sp1)
    store8 SP (sp + 2)
    return $ makeW16 l h

getAMCycles :: AddressMode -> Word64
getAMCycles am =
    case am of
        Implied     -> 0
        Accumulator -> 0
        Immediate   -> 2
        ZeroPage    -> 3
        ZeroPageX   -> 4
        ZeroPageY   -> 4
        Relative    -> 2 -- ++
        Absolute    -> 4
        AbsoluteX   -> 4 -- +
        AbsoluteY   -> 4 -- +
        Indirect    -> 0
        IdxInd      -> 6
        IndIdx      -> 5 -- +
    -- +  = Load instructions get an additional one cycle penalty for crossing page
    --      boundaries during address computations
    -- ++ = Add one cycle if the branch is taken, one more if the branch occurs to different page

-- Determine penalty for page crossing in load instructions
getOperandPageCross :: MonadEmulator m => Instruction -> m Bool
getOperandPageCross (Instruction (OpCode _ am) oper) =
    case oper of 
        [w8]      -> case am of IndIdx -> do l <- load8 . Addr . fromIntegral $ w8
                                             y <- load8 Y
                                             return    $ l + y < l
                                _      -> return False
        (opl:_:[]) -> case am of AbsoluteX -> do x <- load8 X
                                                 return $ opl + x < opl
                                 AbsoluteY -> do y <- load8 Y
                                                 return $ opl + y < opl
                                 _         -> return False
        _          -> return False
getOperandPageCrossPenalty :: MonadEmulator m => Instruction -> m Word64
getOperandPageCrossPenalty inst = (\pagec -> return $ if pagec then 1 else 0) =<< getOperandPageCross inst

-- Determine penalty for page crossing in store instructions. The penalty always
-- occurs for the three modes with 16 bit address computations, regardless of
-- actually having a carry on the address LSB
getStorePageCrossPenalty :: AddressMode -> Word64
getStorePageCrossPenalty am = case am of IndIdx    -> 1
                                         AbsoluteX -> 1
                                         AbsoluteY -> 1
                                         _         -> 0

makeSigned :: Word8 -> Int
makeSigned a = 128 - (fromIntegral $ 128 - a)

samePage :: Word16 -> Word16 -> Bool
samePage a b = (a .&. 128) `xor` (b .&. 128) == 0

execute :: MonadEmulator m => Instruction -> m ()
execute inst@(Instruction (OpCode mn am) _) = do
    let ilen = fromIntegral $ instructionLen inst :: Word16
    case mn of
        LDA -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            a <- loadOperand8 inst
            updateNZ a
            store8 A a
            advCycles $ baseC + penalty
        LDX -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            x <- loadOperand8 inst
            updateNZ x
            store8 X x
            advCycles $ baseC + penalty
        LDY -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            y <- loadOperand8 inst
            updateNZ y
            store8 Y y
            advCycles $ baseC + penalty
        STA -> do
            let baseC = getAMCycles am + getStorePageCrossPenalty am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            a <- load8 A
            storeOperand8 inst a
            advCycles baseC
        STX -> do
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- load8 X
            storeOperand8 inst x
            advCycles baseC
        STY -> do
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            y <- load8 Y
            storeOperand8 inst y
            advCycles baseC
        AND -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            x <- loadOperand8 inst
            a <- load8 A
            let r = x .&. a
            updateNZ r
            store8 A r
            advCycles $ baseC + penalty
        ORA -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            x <- loadOperand8 inst
            a <- load8 A
            let r = x .|. a
            updateNZ r
            store8 A r
            advCycles $ baseC + penalty
        EOR -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            x <- loadOperand8 inst
            a <- load8 A
            let r = x `xor` a
            updateNZ r
            store8 A r
            advCycles $ baseC + penalty
        INC -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- loadOperand8 inst
            let r = x + 1
            updateNZ r
            storeOperand8 inst r
            advCycles baseC
        DEC -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- loadOperand8 inst
            let r = x - 1
            updateNZ r
            storeOperand8 inst r
            advCycles baseC
        ASL -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- loadOperand8 inst
            let carry = testBit x 7
            let r = x `shiftL` 1
            updateNZC r carry
            storeOperand8 inst r
            advCycles baseC
        LSR -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- loadOperand8 inst
            let carry = testBit x 0
            let r = x `shiftR` 1
            updateNZC r carry
            storeOperand8 inst r
            advCycles baseC
        ROL -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- loadOperand8 inst
            carry <- getFlag FC <$> load8 SR
            let r = (x `shiftL` 1) .|. b2W8 carry
            updateNZC r $ testBit x 7
            storeOperand8 inst r
            advCycles baseC
        ROR -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- loadOperand8 inst
            carry <- getFlag FC <$> load8 SR
            let r = (x `shiftR` 1) .|. if carry then 128 else 0
            updateNZC r $ testBit x 0
            storeOperand8 inst r
            advCycles baseC
        JMP -> do
            let baseC = case am of Absolute -> 3; Indirect -> 5; _ -> 0
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            npc <- loadOperand16 inst
            store16 PC npc
            advCycles baseC
        JSR -> do
            let baseC = 6
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            npc <- loadOperand16 inst
            pc  <- load16 PC
            storeStack16 $ pc + ilen - 1
            store16 PC npc
            advCycles baseC
        RTS -> do
            let baseC = 6
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            pc <- loadStack16
            store16 PC $ pc + 1
            advCycles baseC
        TAX -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            a <- load8 A
            updateNZ a
            store8 X a
            advCycles baseC
        TXA -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- load8 X
            updateNZ x
            store8 A x
            advCycles baseC
        TYA -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            y <- load8 Y
            updateNZ y
            store8 A y
            advCycles baseC
        TAY -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            a <- load8 A
            updateNZ a
            store8 Y a
            advCycles baseC
        DEX -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- flip (-) 1 <$> load8 X
            updateNZ x
            store8 X x
            advCycles baseC
        INX -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- (+ 1) <$> load8 X
            updateNZ x
            store8 X x
            advCycles baseC
        DEY -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            y <- flip (-) 1 <$> load8 Y
            updateNZ y
            store8 Y y
            advCycles baseC
        INY -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            y <- (+ 1) <$> load8 Y
            updateNZ y
            store8 Y y
            advCycles baseC
        TXS -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            x <- load8 X
            store8 SP x
            advCycles baseC
        TSX -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sp <- load8 SP
            updateNZ sp
            store8 X sp
            advCycles baseC
        CLC -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sr <- load8 SR
            store8 SR . clearFlag FC $ sr
            advCycles baseC
        SEC -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sr <- load8 SR
            store8 SR . setFlag FC $ sr
            advCycles baseC
        PHP -> do
            let baseC = 3
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sr <- load8 SR
            storeStack8 $ setFlag FB sr
            advCycles baseC
        PHA -> do
            let baseC = 3
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            a <- load8 A
            storeStack8 a
            advCycles baseC
        PLP -> do
            let baseC = 4
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sr <- loadStack8
            store8 SR . clearFlag FB $ sr
            advCycles baseC
        PLA -> do
            let baseC = 4
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            a <- loadStack8
            updateNZ a
            store8 A a
            advCycles baseC
        SED -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sr <- load8 SR
            store8 SR . setFlag FD $ sr
            advCycles baseC
        CLD -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sr <- load8 SR
            store8 SR . clearFlag FD $ sr
            advCycles baseC
        -- SBC is ADC with all argument bits flipped (xor 0xFF, see
        -- http://forums.nesdev.com/viewtopic.php?t=8703), except for the BCD
        -- case. The BCD implementation should be correct for all documented
        -- cases, but does not necessarily match the NMOS 6502 for illegal BCD
        -- values and the 'undefined' NZV flags. The BCD implementation of Py65
        -- is correct in all these cases, should a future reference be needed
        ADC -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            sr <- load8 SR
            op <- loadOperand8 inst
            a  <- load8 A
            let carry = b2W8 $ getFlag FC sr
            let (r, ncarry) = case getFlag FD sr of
                                  False -> let sum = a + op + carry
                                            in (sum, if carry == 1 then sum <= a else sum < a)
                                  -- http://forum.6502.org/viewtopic.php?p=13441
                                  True -> let n0     = carry + (a .&. 0x0F) + (op .&. 0x0F)
                                              hcarry = n0 >= 10
                                              n1     = b2W8 hcarry + (a `shiftR` 4) + (op `shiftR` 4)
                                              n0'    = if hcarry then n0 - 10 else n0
                                              ncarry = n1 >= 10
                                              n1'    = if ncarry then n1 - 10 else n1
                                           in ((n1' `shiftL` 4) + n0', ncarry)
            store8 A r
            -- http://forums.nesdev.com/viewtopic.php?p=60520
            let overflow = (a `xor` r) .&. (op `xor` r) .&. 0x80 /= 0
            store8 SR . modifyFlag FC ncarry . modifyFlag FV overflow . setNZ r $ sr
            advCycles $ baseC + penalty
        SBC -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            sr <- load8 SR
            op <- loadOperand8 inst
            a  <- load8 A
            let carry = b2W8 . not $ getFlag FC sr
            let (r, ncarry) = case getFlag FD sr of
                                  False -> let sum = a - (op + carry)
                                            in (sum, not $ if carry == 1 then sum >= a else sum > a)
                                  -- http://forum.6502.org/viewtopic.php?p=13441
                                  True -> let ix = fromIntegral op :: Int
                                              xdec  = ((ix `shiftR` 4) * 10) + (ix .&. 0x0F)
                                                      + fromIntegral carry
                                              ia = fromIntegral a :: Int
                                              adec  = ((ia `shiftR` 4) * 10) + (ia .&. 0x0F) - xdec
                                              ncarry = adec < 0
                                              adec' = if ncarry then adec + 100 else adec
                                              r = ((adec' `div` 10) `shiftL` 4) + (adec' `mod` 10)
                                           in (fromIntegral r :: Word8, not ncarry)
            store8 A r
            -- http://forums.nesdev.com/viewtopic.php?p=60520
            let overflow = (a `xor` r) .&. (op `xor` r) .&. 0x80 == 0
            store8 SR . modifyFlag FC ncarry . modifyFlag FV overflow . setNZ r $ sr
            advCycles $ baseC + penalty
        CMP -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            op <- loadOperand8 inst
            a  <- load8 A
            sr <- load8 SR
            let isN = testBit a 7
            store8 SR
                . modifyFlag FN isN
                . modifyFlag FZ (a == op)
                . modifyFlag FC (a >= op)
                $ sr
            advCycles $ baseC + penalty
        BEQ -> do
            f    <- getFlag FZ <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BNE -> do
            f    <- not . getFlag FZ <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        CPX -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            op <- loadOperand8 inst
            x  <- load8 X
            sr <- load8 SR
            let isN = testBit x 7
            store8 SR
                . modifyFlag FN isN
                . modifyFlag FZ (x == op)
                . modifyFlag FC (x >= op)
                $ sr
            advCycles $ baseC + penalty
        CPY -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "" :: String)
            update16 PC (ilen +)
            op <- loadOperand8 inst
            y  <- load8 Y
            sr <- load8 SR
            let isN = testBit y 7
            store8 SR
                . modifyFlag FN isN
                . modifyFlag FZ (y == op)
                . modifyFlag FC (y >= op)
                $ sr
            advCycles $ baseC + penalty
        BIT -> do
            let baseC = getAMCycles am
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            a  <- load8 A
            x  <- loadOperand8 inst
            sr <- load8 SR
            let r = a .&. x
            store8 SR
                . modifyFlag FZ (r == 0)
                . modifyFlag FV (testBit x 6)
                . modifyFlag FN (testBit x 7)
                $ sr
            advCycles baseC
        BPL -> do
            f    <- not . getFlag FN <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BMI -> do
            f    <- not . getFlag FN <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BVC -> do
            f    <- not . getFlag FV <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BVS -> do
            f    <- getFlag FV <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BCC -> do
            f    <- not . getFlag FC <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BCS -> do
            f    <- getFlag FC <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = if offs > 0 then pc + fromIntegral offs else pc - fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace . B8.pack $ printf "\n%s (%ib, %i%sC): " (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+1+1"; _ -> "" :: String)
            store16 PC $ if f then dest else pc
            advCycles $ baseC + penalty
        CLV -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            sr <- load8 SR
            store8 SR . clearFlag FV $ sr
            advCycles baseC
        NOP -> do
            let baseC = 2
            trace . B8.pack $ printf "\n%s (%ib, %iC): " (show inst) ilen baseC
            update16 PC (ilen +)
            advCycles baseC
        DCB _ -> do
            trace . B8.pack $ printf "\n%s (Illegal OpCode, %ib, %iC): " (show inst) ilen (1 :: Int)
            update16 PC (1 +)
            advCycles 1
        _ -> error "Instruction Not Implemented" -- update16 PC (1 +) >> advCycles 1
    cpustate <- showCPUState
    trace . B8.pack $ "\n" ++ cpustate ++ "\n"

