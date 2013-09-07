
{-# LANGUAGE ViewPatterns, LambdaCase #-}

module Execution ( execute
                 , detectLoopOnPC
                 , store8Trace
                 , store16Trace
                 ) where

-- The actual emulation of all 6502 instructions running inside of MonadEmulator

import MonadEmulator
import Instruction
import Util

import Data.Word (Word8, Word16, Word64)
import Text.Printf
import Data.Bits (testBit, (.&.), (.|.), xor, shiftL, shiftR, complement)
import Control.Applicative ((<$>), (<*>))

store8Trace :: MonadEmulator m => LoadStore -> Word8  -> m ()
store8Trace ls val = do
    trace $ printf "%02X→%s " val (show ls)
    store8 ls val
store16Trace :: MonadEmulator m => LoadStore -> Word16 -> m ()
store16Trace ls val = do
    trace $ printf "%04X→%s " val (show ls)
    store16 ls val

-- Functions for loading and storing 8 bit operands for any instruction. Illegal
-- instructions (writing to an Immediate operand, reading using the Indirect
-- mode, having no operand data for anything but Immediate/Accumulator, etc.)
-- will result in an error trace and a dummy return value

getOperandAddr8 :: MonadEmulator m => Instruction -> m LoadStore
getOperandAddr8 inst@(Instruction (viewOpCode -> OpCode _ _ am) oper) =
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
    err = trace ("getOperandAddr8: AM/OpLen Error: " ++ show inst) >> return A

traceNoOpLoad :: MonadEmulator m => m ()
traceNoOpLoad = trace "-         "

loadOperand8 :: MonadEmulator m => Instruction -> m Word8
loadOperand8 inst@(Instruction (viewOpCode -> OpCode _ _ am) oper) =
    case oper of
        [w8] -> case am of Immediate -> traceNoOpLoad >> return w8
                           Relative  ->                  return w8
                           _         -> loadAndTrace
        _    ->                         loadAndTrace
  where
    loadAndTrace =  do ls <- getOperandAddr8 inst
                       w8 <- load8 ls
                       case ls of
                           -- Trace operands where value / address might not be
                           -- immediately obvious from the instruction
                           Addr addr -> trace $ printf "%04X→%02X   " addr w8
                           _         -> traceNoOpLoad
                       return w8

storeOperand8 :: MonadEmulator m => Instruction -> Word8 -> m ()
storeOperand8 inst val = (\ls -> store8Trace ls val) =<< getOperandAddr8 inst

-- There are no instructions storing 16 bit operands, and the only instructions
-- that load them for actually doing anything with them besides looking up an 8
-- bit value (covered by loadOperand8) are JMP / JSR with Absolute / Indirect
-- addressing

loadOperand16 :: MonadEmulator m => Instruction -> m Word16
loadOperand16 inst@(Instruction (viewOpCode -> OpCode _ _ am) oper) =
    case oper of
        (opl:oph:[]) -> case am of
            Absolute  -> do traceNoOpLoad
                            return $ makeW16 opl oph
            Indirect  -> do l <- load8 . Addr $ makeW16  opl      oph
                            -- The NMOS 6502 actually does it like this
                            h <- load8 . Addr $ makeW16 (opl + 1) oph
                            let w16 = makeW16 l h
                            trace $ printf "%04X→%04X " (makeW16 opl oph) w16
                            return w16
            _         -> err
        _            -> err
  where
    err = trace ("loadOperand16: AM/OpLen Error: " ++ show inst) >> return 0

update16 :: MonadEmulator m => LoadStore -> (Word16 -> Word16) -> m ()
update16 ls f = load16 ls >>= return . f >>= store16Trace ls

setNZ :: Word8 -> Word8 -> Word8
setNZ x sr =
    let isN = testBit x 7
        isZ = x == 0
     in modifyFlag FN isN . modifyFlag FZ isZ $ sr

updateNZ :: MonadEmulator m => Word8 -> m ()
updateNZ x = do
    sr <- load8 SR
    store8Trace SR $ setNZ x sr

setNZC :: Word8 -> Bool -> Word8 -> Word8
setNZC x carry sr =
    let isN = testBit x 7
        isZ = x == 0
     in modifyFlag FN isN . modifyFlag FZ isZ .  modifyFlag FC carry $ sr

updateNZC :: MonadEmulator m => Word8 -> Bool -> m ()
updateNZC x carry = do
    sr <- load8 SR
    store8Trace SR . setNZC x carry $ sr

storeStack8 :: MonadEmulator m => Word8 -> m ()
storeStack8 w8 = do
    sp <- load8 SP
    store8Trace (Addr $ 0x0100 + fromIntegral sp) w8
    store8Trace SP (sp - 1)

storeStack16 :: MonadEmulator m => Word16 -> m ()
storeStack16 w16 = do
    let (l, h) = splitW16 w16
    sp <- load8 SP
    let sp1 = sp
    let sp2 = sp - 1
    store8Trace (Addr $ 0x0100 + fromIntegral sp1) h
    store8Trace (Addr $ 0x0100 + fromIntegral sp2) l
    store8Trace SP (sp - 2)

loadStack8 :: MonadEmulator m => m Word8
loadStack8 = do
    sp <- (+) 1 <$> load8 SP
    w8 <- load8 (Addr $ 0x0100 + fromIntegral sp )
    store8Trace SP sp
    return w8

loadStack16 :: MonadEmulator m => m Word16
loadStack16 = do
    sp <- load8 SP
    let sp1 = sp + 1
    let sp2 = sp + 2
    l <- load8 (Addr $ 0x0100 + fromIntegral sp1)
    h <- load8 (Addr $ 0x0100 + fromIntegral sp2)
    store8Trace SP (sp + 2)
    return $ makeW16 l h

getAMCycles :: AddressMode -> Word64
getAMCycles =
    \case
        Implied     -> 2
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
getOperandPageCross (Instruction (viewOpCode -> OpCode _ _ am) oper) =
    case oper of
        [w8]       -> case am of IndIdx -> do l <- load8 . Addr . fromIntegral $ w8
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
getOperandPageCrossPenalty inst =
    (\pagec -> return $ if pagec then 1 else 0) =<< getOperandPageCross inst

-- Determine penalty for page crossing in store instructions. The penalty always
-- occurs for the three modes with 16 bit address computations, regardless of
-- actually having a carry on the address LSB
getStorePageCrossPenalty :: AddressMode -> Word64
getStorePageCrossPenalty = \case IndIdx    -> 1
                                 AbsoluteX -> 1
                                 AbsoluteY -> 1
                                 _         -> 0

-- Two's complement to signed integer conversion
makeSigned :: Word8 -> Int
makeSigned a = if (a .&. 0x80 /= 0) then -(fromIntegral $ complement a + 1) else fromIntegral a

samePage :: Word16 -> Word16 -> Bool
samePage a b = (a .&. 0xFF00) == (b .&. 0xFF00)

-- Detect if the current instruction jumps to itself (infinite loop)
detectLoopOnPC :: MonadEmulator m => Instruction -> m Bool
detectLoopOnPC inst =
    case inst of
        Instruction (viewOpCode -> OpCode _ JMP _) _      ->
            -- We don't want to have an operand trace here
            runNoTrace $ (==) <$> load16 PC <*> loadOperand16 inst
        Instruction (viewOpCode -> OpCode _ BCS _) [0xFE] -> return .       getFlag FC =<< load8 SR
        Instruction (viewOpCode -> OpCode _ BCC _) [0xFE] -> return . not . getFlag FC =<< load8 SR
        Instruction (viewOpCode -> OpCode _ BEQ _) [0xFE] -> return .       getFlag FZ =<< load8 SR
        Instruction (viewOpCode -> OpCode _ BNE _) [0xFE] -> return . not . getFlag FZ =<< load8 SR
        Instruction (viewOpCode -> OpCode _ BMI _) [0xFE] -> return .       getFlag FN =<< load8 SR
        Instruction (viewOpCode -> OpCode _ BPL _) [0xFE] -> return . not . getFlag FN =<< load8 SR
        Instruction (viewOpCode -> OpCode _ BVS _) [0xFE] -> return .       getFlag FV =<< load8 SR
        Instruction (viewOpCode -> OpCode _ BVC _) [0xFE] -> return . not . getFlag FV =<< load8 SR
        Instruction (viewOpCode -> OpCode _ KIL _) _      -> return True
        _                                                 -> return False

useBCD :: MonadEmulator m => Word8 -> m Bool
useBCD sr = (\model -> getFlag FD sr && (model /= NES_2A03)) <$> getModel

-- Check for signed overflow. Look at the sign of the result and operands, we
-- have an overflow when the sign of the result matches neither operand
-- (Also see http://forums.nesdev.com/viewtopic.php?p=60520)
overflow :: Word8 -> Word8 -> Word8 -> Bool
overflow op1 op2 r = (op1 `xor` r) .&. (op2 `xor` r) .&. 0x80 /= 0

-- Core of the ADC/SBC instructions and the illegal ones reusing their
-- components. Computes the result and all four flags. Decimal mode is handled
-- as well, including invalid BCD values and the 'undocumented' three flags.
-- This is neither the fastest nor the most compact way of doing this, but it
-- follows the way the actual 6502 works a bit more closely and makes it easier
-- to understand why these instructions work as they do, especially for the
-- undocumented / edge cases
--
-- References:
--
-- http://www.6502.org/tutorials/decimal_mode.html
-- http://en.wikipedia.org/wiki/Binary-coded_decimal
-- http://www.viceteam.org/plain/64doc.txt
-- http://sourceforge.net/p/vice-emu/code/27740/tree/trunk/vice/src/6510core.c
--
--                                            result N     V     Z     C
adcCore :: Word8 -> Word8 -> Bool -> Bool -> (Word8, Bool, Bool, Bool, Bool)
adcCore a op carry bcd =
    let r = op + a + b2W8 carry
        -- Z is computed before any BCD fixup / unaffected by decimal setting,
        -- just check if what we got is zero
        z = r == 0
     in if bcd
            then let -- Compute lower nibble
                     nl     = (a .&. 0x0F) + (op .&. 0x0F) + b2W8 carry
                     -- BCD fixup and carry from lower nibble
                     nl'    = nl + if nl > 0x09 then 0x06 else 0
                     hcarry = if nl' > 0x0F then 0x10 else 0
                     -- Compute upper nibble
                     nh     = (fromIntegral a .&. 0xF0) + (fromIntegral op .&. 0xF0) :: Int
                     -- Intermediate sum with only the lower nibble having the decimal
                     -- adjust, needed for N & V flags
                     rFixNL = (fromIntegral nl' .&. 0x0F) + nh + hcarry :: Int
                     -- N & V get computed after BCD fixup for lower nibble
                     n      = rFixNL .&. 0x80 /= 0
                     v      = overflow a op $ fromIntegral rFixNL
                     -- Do decimal adjust for upper nibble
                     rBCD   = rFixNL + if rFixNL .&. 0x1F0 > 0x90 then 0x60 else 0
                     -- Carry done on fully adjusted result, only 'valid' flag in BCD
                     -- on the NMOS 6502
                     c      = rBCD .&. 0xFF0 > 0xF0
                  in (fromIntegral rBCD, n, v, z, c)
            else let v = overflow a op r
                     n = r .&. 0x80 /= 0
                     c = if carry then r <= a else r < a
                  in (r, n, v, z, c)
--                                            result N     V     Z     C
sbcCore :: Word8 -> Word8 -> Bool -> Bool -> (Word8, Bool, Bool, Bool, Bool)
sbcCore a op carry bcd =
    let r = a - op - (b2W8 $ not carry)
        -- Flags are computed before any BCD fixup / unaffected by decimal setting
        n = r .&. 0x80 /= 0
        v = overflow a (complement op) r
        z = r == 0
        c = if carry then r <= a else r < a
     in if bcd
            then let -- Compute lower nibble
                     nl     = (a .&. 0x0F) - (op .&. 0x0F) - (b2W8 $ not carry)
                     -- BCD fixup and carry from lower nibble
                     hcarry = nl .&. 0x10 /= 0
                     nl'    = nl - if hcarry then 0x06 else 0
                     -- Compute upper nibble with carry
                     nh     = fromIntegral (a  .&. 0xF0) -
                              fromIntegral (op .&. 0xF0) -
                              if hcarry then 0x10 else 0
                              :: Int
                     -- Upper nibble BCD fixup
                     nh'    = nh - if nh .&. 0x100 /= 0 then 0x60 else 0
                     rBCD   = (nl' .&. 0x0F) .|. fromIntegral nh'
                  in (rBCD, n, v, z, c)
            else (r, n, v, z, c)

{-# INLINE execute #-}
execute :: MonadEmulator m => Instruction -> m ()
execute inst@(Instruction (viewOpCode -> OpCode w mn am) _) = do
    traceM $ do
        cpustate <- showCPUState False
        return $ "\n" ++ cpustate ++ " "
    let ilen = fromIntegral $ instructionLen inst :: Word16
    case mn of
        LDA -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            a <- loadOperand8 inst
            updateNZ a
            store8Trace A a
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        LDX -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            x <- loadOperand8 inst
            updateNZ x
            store8Trace X x
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        LDY -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            y <- loadOperand8 inst
            updateNZ y
            store8Trace Y y
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        STA -> do
            let baseC = getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            storeOperand8 inst a
            update16 PC (ilen +)
            advCycles baseC
        STX -> do
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- load8 X
            storeOperand8 inst x
            update16 PC (ilen +)
            advCycles baseC
        STY -> do
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            y <- load8 Y
            storeOperand8 inst y
            update16 PC (ilen +)
            advCycles baseC
        AND -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            x <- loadOperand8 inst
            a <- load8 A
            let r = x .&. a
            updateNZ r
            store8Trace A r
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        ORA -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            x <- loadOperand8 inst
            a <- load8 A
            let r = x .|. a
            updateNZ r
            store8Trace A r
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        EOR -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            x <- loadOperand8 inst
            a <- load8 A
            let r = x `xor` a
            updateNZ r
            store8Trace A r
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        INC -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            x <- loadOperand8 inst
            let r = x + 1
            updateNZ r
            storeOperand8 inst r
            update16 PC (ilen +)
            advCycles baseC
        DEC -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            x <- loadOperand8 inst
            let r = x - 1
            updateNZ r
            storeOperand8 inst r
            update16 PC (ilen +)
            advCycles baseC
        ASL -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            x <- loadOperand8 inst
            let carry = testBit x 7
            let r = x `shiftL` 1
            updateNZC r carry
            storeOperand8 inst r
            update16 PC (ilen +)
            advCycles baseC
        LSR -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            x <- loadOperand8 inst
            let carry = testBit x 0
            let r = x `shiftR` 1
            updateNZC r carry
            storeOperand8 inst r
            update16 PC (ilen +)
            advCycles baseC
        ROL -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            x <- loadOperand8 inst
            carry <- getFlag FC <$> load8 SR
            let r = (x `shiftL` 1) .|. b2W8 carry
            updateNZC r $ testBit x 7
            storeOperand8 inst r
            update16 PC (ilen +)
            advCycles baseC
        ROR -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            x <- loadOperand8 inst
            carry <- getFlag FC <$> load8 SR
            let r = (x `shiftR` 1) .|. if carry then 0x80 else 0
            updateNZC r $ testBit x 0
            storeOperand8 inst r
            update16 PC (ilen +)
            advCycles baseC
        JMP -> do
            let baseC = case am of Absolute -> 3; Indirect -> 5; _ -> 0
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            npc <- loadOperand16 inst
            store16Trace PC npc
            advCycles baseC
        JSR -> do
            let baseC = 6
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            npc <- loadOperand16 inst
            pc  <- load16 PC
            storeStack16 $ pc + ilen - 1
            store16Trace PC npc
            advCycles baseC
        RTS -> do
            let baseC = 6
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            pc <- loadStack16
            store16Trace PC $ pc + 1
            advCycles baseC
        TAX -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            updateNZ a
            store8Trace X a
            update16 PC (ilen +)
            advCycles baseC
        TXA -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- load8 X
            updateNZ x
            store8Trace A x
            update16 PC (ilen +)
            advCycles baseC
        TYA -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            y <- load8 Y
            updateNZ y
            store8Trace A y
            update16 PC (ilen +)
            advCycles baseC
        TAY -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            updateNZ a
            store8Trace Y a
            update16 PC (ilen +)
            advCycles baseC
        DEX -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- flip (-) 1 <$> load8 X
            updateNZ x
            store8Trace X x
            update16 PC (ilen +)
            advCycles baseC
        INX -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- (+ 1) <$> load8 X
            updateNZ x
            store8Trace X x
            update16 PC (ilen +)
            advCycles baseC
        DEY -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            y <- flip (-) 1 <$> load8 Y
            updateNZ y
            store8Trace Y y
            update16 PC (ilen +)
            advCycles baseC
        INY -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            y <- (+ 1) <$> load8 Y
            updateNZ y
            store8Trace Y y
            update16 PC (ilen +)
            advCycles baseC
        TXS -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- load8 X
            store8Trace SP x
            update16 PC (ilen +)
            advCycles baseC
        TSX -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sp <- load8 SP
            updateNZ sp
            store8Trace X sp
            update16 PC (ilen +)
            advCycles baseC
        CLC -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            store8Trace SR . clearFlag FC $ sr
            update16 PC (ilen +)
            advCycles baseC
        SEC -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            store8Trace SR . setFlag FC $ sr
            update16 PC (ilen +)
            advCycles baseC
        PHP -> do
            let baseC = 3
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            storeStack8 $ setFlag FB sr
            update16 PC (ilen +)
            advCycles baseC
        PHA -> do
            let baseC = 3
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            storeStack8 a
            update16 PC (ilen +)
            advCycles baseC
        PLP -> do
            let baseC = 4
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- loadStack8
            store8Trace SR . clearFlag FB . setFlag F1 $ sr
            update16 PC (ilen +)
            advCycles baseC
        PLA -> do
            let baseC = 4
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- loadStack8
            updateNZ a
            store8Trace A a
            update16 PC (ilen +)
            advCycles baseC
        SED -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            update16 PC (ilen +)
            store8Trace SR . setFlag FD $ sr
            advCycles baseC
        CLD -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            store8Trace SR . clearFlag FD $ sr
            update16 PC (ilen +)
            advCycles baseC
        SEI -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            store8Trace SR . setFlag FI $ sr
            update16 PC (ilen +)
            advCycles baseC
        CLI -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            store8Trace SR . clearFlag FI $ sr
            update16 PC (ilen +)
            advCycles baseC
        ADC -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            sr  <- load8 SR
            op  <- loadOperand8 inst
            a   <- load8 A
            bcd <- useBCD sr
            let carry = getFlag FC sr
            let (r, n, v, z, c) = adcCore a op carry bcd
            store8Trace A r
            store8Trace SR
                . modifyFlag FN n
                . modifyFlag FV v
                . modifyFlag FZ z
                . modifyFlag FC c
                $ sr
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        SBC -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s %s%ib%iC%s "
                w (show inst)
                (if w == 0xEB then "I" else "O") -- 0xEB is the only illegal variant
                ilen
                baseC
                (if penalty /= 0 then "+1"  else "  ")
            sr  <- load8 SR
            op  <- loadOperand8 inst
            a   <- load8 A
            bcd <- useBCD sr
            let carry = getFlag FC sr
            let (r, n, v, z, c) = sbcCore a op carry bcd
            store8Trace A r
            store8Trace SR
                . modifyFlag FN n
                . modifyFlag FV v
                . modifyFlag FZ z
                . modifyFlag FC c
                $ sr
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        CMP -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            op <- loadOperand8 inst
            a  <- load8 A
            sr <- load8 SR
            let isN = testBit (a - op) 7
            store8Trace SR
                . modifyFlag FN isN
                . modifyFlag FZ (a == op)
                . modifyFlag FC (a >= op)
                $ sr
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        CPX -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            op <- loadOperand8 inst
            x  <- load8 X
            sr <- load8 SR
            let isN = testBit (x - op) 7
            store8Trace SR
                . modifyFlag FN isN
                . modifyFlag FZ (x == op)
                . modifyFlag FC (x >= op)
                $ sr
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        CPY -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            op <- loadOperand8 inst
            y  <- load8 Y
            sr <- load8 SR
            let isN = testBit (y - op) 7
            store8Trace SR
                . modifyFlag FN isN
                . modifyFlag FZ (y == op)
                . modifyFlag FC (y >= op)
                $ sr
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        BIT -> do
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            a  <- load8 A
            x  <- loadOperand8 inst
            sr <- load8 SR
            let r = a .&. x
            store8Trace SR
                . modifyFlag FZ (r == 0)
                . modifyFlag FV (testBit x 6)
                . modifyFlag FN (testBit x 7)
                $ sr
            update16 PC (ilen +)
            advCycles baseC
        BEQ -> do
            f    <- getFlag FZ <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BNE -> do
            f    <- not . getFlag FZ <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BPL -> do
            f    <- not . getFlag FN <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BMI -> do
            f    <- getFlag FN <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BVC -> do
            f    <- not . getFlag FV <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BVS -> do
            f    <- getFlag FV <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BCC -> do
            f    <- not . getFlag FC <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        BCS -> do
            f    <- getFlag FC <$> load8 SR
            oper <- loadOperand8 inst
            pc   <- (+) ilen <$> load16 PC
            let offs    = makeSigned oper
                dest    = pc + fromIntegral offs
                pagecr  = (not $ samePage dest pc) && f
                baseC   = getAMCycles Relative
                penalty = fromIntegral $ b2W8 f + b2W8 pagecr :: Word64
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (case penalty of 1 -> "+1"; 2 -> "+2"; _ -> "  ")
            traceNoOpLoad
            store16Trace PC $ if f then dest else pc
            advCycles $ baseC + penalty
        CLV -> do
            let baseC = 2
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- load8 SR
            store8Trace SR . clearFlag FV $ sr
            update16 PC (ilen +)
            advCycles baseC
        NOP -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s %s%ib%iC%s "
                w (show inst)
                (if w /= 0xEA then "I" else "O") -- 0xEA is the only official NOP
                ilen
                baseC
                (if penalty /= 0 then "+1"  else "  ")
            traceNoOpLoad
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        RTI -> do
            let baseC = 6
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            sr <- loadStack8
            store8Trace SR . clearFlag FB . setFlag F1 $ sr
            pc <- loadStack16
            store16Trace PC pc
            advCycles baseC
        BRK -> do
            let baseC = 7
            trace $ printf "%02X:%-11s O%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            pc <- load16 PC
            storeStack16 $ pc + ilen + 1 -- Don't forget the padding byte
            sr <- load8 SR
            storeStack8 $ setFlag FB sr
            store8Trace SR . setFlag FI $ sr
            ivec <- load16 $ Addr 0xFFFE
            store16Trace PC ivec
            advCycles baseC
        KIL -> do
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen (1 :: Int)
            traceNoOpLoad
            advCycles 1
        LAX -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s %s%ib%iC%s "
                w (show inst)
                -- 0xAB / LAX Immediate is highly unstable, just execute it as
                -- expected, unlikely anybody relies on its exact behavior
                (if am == Immediate then "U" else "I")
                ilen
                baseC
                (if penalty /= 0 then "+1"  else "  ")
            op <- loadOperand8 inst
            updateNZ op
            store8Trace A op
            store8Trace X op
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        SAX -> do
            let baseC = getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            x <- load8 X
            storeOperand8 inst $ a .&. x
            update16 PC (ilen +)
            advCycles baseC
        DCP -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            let m = op - 1
            storeOperand8 inst m
            a  <- load8 A
            sr <- load8 SR
            let isN = testBit (a - m) 7
            store8Trace SR
                . modifyFlag FN isN
                . modifyFlag FZ (a == m)
                . modifyFlag FC (a >= m)
                $ sr
            update16 PC (ilen +)
            advCycles baseC
        ISC -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op  <- loadOperand8 inst
            let m = op + 1
            storeOperand8 inst m
            sr  <- load8 SR
            let carry = getFlag FC sr
            a   <- load8 A
            bcd <- useBCD sr
            let (r, n, v, z, c) = sbcCore a m carry bcd
            store8Trace SR
                . modifyFlag FN n
                . modifyFlag FV v
                . modifyFlag FZ z
                . modifyFlag FC c
                $ sr
            store8Trace A r
            update16 PC (ilen +)
            advCycles baseC
        RLA -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            carry <- getFlag FC <$> load8 SR
            let rol = (op `shiftL` 1) .|. b2W8 carry
            storeOperand8 inst rol
            a <- load8 A
            let and' = rol .&. a
            updateNZC and' $ testBit op 7
            store8Trace A and'
            update16 PC (ilen +)
            advCycles baseC
        RRA -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            sr <- load8 SR
            a  <- load8 A
            let rorcarry = getFlag FC sr
            let ror = (op `shiftR` 1) .|. if rorcarry then 0x80 else 0
            storeOperand8 inst ror
            let carry = testBit op 0
            bcd <- useBCD sr
            let (r, n, v, z, c) = adcCore a ror carry bcd
            store8Trace A r
            store8Trace SR
                . modifyFlag FN n
                . modifyFlag FV v
                . modifyFlag FZ z
                . modifyFlag FC c
                $ sr
            update16 PC (ilen +)
            advCycles baseC
        SLO -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            let asl = op `shiftL` 1
            storeOperand8 inst asl
            a <- load8 A
            let r = asl .|. a
            updateNZC r $ testBit op 7
            store8Trace A r
            update16 PC (ilen +)
            advCycles baseC
        SRE -> do
            let baseC = 2 + getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            let lsr = op `shiftR` 1
            storeOperand8 inst lsr
            a <- load8 A
            let r = lsr `xor` a
            updateNZC r $ testBit op 0
            store8Trace A r
            update16 PC (ilen +)
            advCycles baseC
        ANC -> do
            let baseC = 2 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            a  <- load8 A
            sr <- load8 SR
            let r = op .&. a
            store8Trace SR . modifyFlag FC (testBit r 7) . setNZ r $ sr
            store8Trace A r
            update16 PC (ilen +)
            advCycles baseC
        ALR -> do
            let baseC = 2 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            a  <- load8 A
            let and'  = op .&. a
                r     = and' `shiftR` 1
                carry = testBit and' 0
            updateNZC r $ carry
            store8Trace A r
            update16 PC (ilen +)
            advCycles baseC
        ARR -> do
            let baseC = 2 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op  <- loadOperand8 inst
            sr  <- load8 SR
            a   <- load8 A
            bcd <- useBCD sr
            let and'      = op .&. a
                carry     = getFlag FC sr
                ror       = (and' `shiftR` 1) .|. if carry then 0x80 else 0
                z         = ror == 0
                n         = carry -- 7th bit (sign) is always the carry
                (r, v, c) =
                      if bcd
                    then let -- If the lower nibble of the AND result plus its
                             -- lowest bit is greater than 5, add 6 to the lower
                             -- nibble in the ROR result. The BCD fixup may
                             -- overflow, but the high nibble won't get a carry
                             fixNL  =   if (and' .&. 0x0F) + (and' .&. 0x01) > 0x05
                                      then (ror .&. 0xF0) .|. ((ror + 0x06) .&. 0x0F)
                                      else ror
                             -- If the upper nibble of the AND result plus its
                             -- lowest bit is greater than 5, add 6 to the upper
                             -- nibble in the ROR result and set the carry
                             ncarry = (fromIntegral and' .&. 0xF0 :: Int) +
                                      (fromIntegral and' .&. 0x10 :: Int) > 0x50
                             fixNH  =    if ncarry
                                       then (fromIntegral fixNL .&. 0x0F) .|.
                                            ((fromIntegral fixNL + 0x60) .&. 0xF0) :: Int
                                       else fromIntegral fixNL
                          in ( {- r -} fromIntegral fixNH
                             , {- v -} (ror `xor` a) .&. 0x40 /= 0 -- XOR Bit6
                             , {- c -} ncarry
                             )
                    else ( {- r -} ror
                         , {- v -} (testBit ror 5) /= (testBit ror 6) -- Bit5 ^ Bit6
                         , {- c -} testBit ror 6
                         )
            store8Trace A r
            store8Trace SR
                . modifyFlag FN n
                . modifyFlag FV v
                . modifyFlag FZ z
                . modifyFlag FC c
                $ sr
            update16 PC (ilen +)
            advCycles baseC
        XAA -> do
            let baseC = 2 :: Word64
            trace $ printf "%02X:%-11s U%ib%iC   " w (show inst) ilen baseC
            -- This opcode is not stable on a real 6502 and its result depends
            -- on analog effects and varies between different CPUs and operating
            -- conditions. This is a valid, if not entirely accurate
            -- implementation of its operation
            op <- loadOperand8 inst
            a  <- load8 A
            x  <- load8 X
            let magic = 0xFF
                r     = (a .|. magic) .&. x .&. op
            store8Trace A r
            updateNZ r
            update16 PC (ilen +)
            advCycles baseC
        AHX -> do
            let baseC = getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            x <- load8 X
            y <- load8 Y
            (l, h) <-
                (\case Instruction (viewOpCode -> OpCode _ AHX AbsoluteY) (l:h:[]) -> return (l, h)
                       Instruction (viewOpCode -> OpCode _ AHX IndIdx)    (zp:[])  -> do
                          indL <- load8 . Addr . fromIntegral $ zp
                          indH <- load8 . Addr . fromIntegral $ zp + 1
                          return (indL, indH)
                       _ -> trace ("AHX: AM/OpLen Error: " ++ show inst) >> return (0, 0)
                ) inst
            let r = a .&. x .&. (h + 1)
            -- The result to be written is used as MSB of the
            -- storage address if we cross a page boundary
            let hiaddr = if l + y < l then r else h
                addr   = fromIntegral (l + y) .|. (fromIntegral hiaddr `shiftL` 8)
            store8Trace (Addr addr) r
            update16 PC (ilen +)
            advCycles baseC
        TAS -> do
            let baseC = getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            x <- load8 X
            y <- load8 Y
            let sp = x .&. a
            store8Trace SP sp
            (l, h) <-
                (\case Instruction (viewOpCode -> OpCode _ TAS AbsoluteY) (l:h:[]) -> return (l, h)
                       _ -> trace ("TAS: AM/OpLen Error: " ++ show inst) >> return (0, 0)
                ) inst
            let r = sp .&. (h + 1)
            -- The result to be written is used as MSB of the
            -- storage address if we cross a page boundary
            let hiaddr = if l + y < l then r else h
                addr   = fromIntegral (l + y) .|. (fromIntegral hiaddr `shiftL` 8)
            store8Trace (Addr addr) r
            update16 PC (ilen +)
            advCycles baseC
        SHX -> do
            let baseC = getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- load8 X
            y <- load8 Y
            (l, h) <-
                (\case Instruction (viewOpCode -> OpCode _ SHX AbsoluteY) (l:h:[]) -> return (l, h)
                       _ -> trace ("SHX: AM/OpLen Error: " ++ show inst) >> return (0, 0)
                ) inst
            let r = x .&. (h + 1)
            -- The result to be written is used as MSB of the
            -- storage address if we cross a page boundary
            let hiaddr = if l + y < l then r else h
                addr   = fromIntegral (l + y) .|. (fromIntegral hiaddr `shiftL` 8)
            store8Trace (Addr addr) r
            update16 PC (ilen +)
            advCycles baseC
        SHY -> do
            let baseC = getAMCycles am + getStorePageCrossPenalty am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- load8 X
            y <- load8 Y
            (l, h) <-
                (\case Instruction (viewOpCode -> OpCode _ SHY AbsoluteX) (l:h:[]) -> return (l, h)
                       _ -> trace ("SHY: AM/OpLen Error: " ++ show inst) >> return (0, 0)
                ) inst
            let r = y .&. (h + 1)
            -- The result to be written is used as MSB of the
            -- storage address if we cross a page boundary
            let hiaddr = if l + x < l then r else h
                addr   = fromIntegral (l + x) .|. (fromIntegral hiaddr `shiftL` 8)
            store8Trace (Addr addr) r
            update16 PC (ilen +)
            advCycles baseC
        LAS -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s I%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            op <- loadOperand8 inst
            sp <- load8 SP
            let r = sp .&. op
            store8Trace SP r
            store8Trace A  r
            store8Trace X  r
            updateNZ r
            update16 PC (ilen +)
            advCycles $ baseC + penalty
        AXS -> do
            let baseC = 2 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            a  <- load8 A
            x  <- load8 X
            let r = (a .&. x) - op
            store8Trace X r
            updateNZC r $ (a .&. x) >= op
            update16 PC (ilen +)
            advCycles baseC

