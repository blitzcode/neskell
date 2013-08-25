
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

-- Functions for loading and storing 8 bit operands for any instruction.
-- Illegal instructions (writing to an Immediate operand, reading using the
-- Indirect mode, having no operand data for anything but
-- Immediate/Accumulator, etc.) will result in an error trace and a dummy
-- return value

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
        [w8] -> case am of Immediate ->  traceNoOpLoad >> return w8
                           Relative  ->                   return w8
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
getStorePageCrossPenalty = \case IndIdx    -> 1
                                 AbsoluteX -> 1
                                 AbsoluteY -> 1
                                 _         -> 0

-- Two's complement to signed integer conversion
makeSigned :: Word8 -> Int
makeSigned a = if (a .&. 128 /= 0) then -(fromIntegral $ complement a + 1) else fromIntegral a

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
            let r = (x `shiftR` 1) .|. if carry then 128 else 0
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
        -- SBC is ADC with all argument bits flipped (xor 0xFF, see
        -- http://forums.nesdev.com/viewtopic.php?t=8703), except for the BCD
        -- case. The BCD implementation should be correct for all documented
        -- cases, but does not necessarily match the NMOS 6502 for illegal BCD
        -- values and the 'undefined' NZV flags. The BCD implementation of Py65
        -- is correct in all these cases, should a future reference be needed
        ADC -> do
            penalty <- getOperandPageCrossPenalty inst
            let baseC = getAMCycles am
            trace $ printf "%02X:%-11s O%ib%iC%s " w (show inst) ilen baseC
                (if penalty /= 0 then "+1"  else "  ")
            sr <- load8 SR
            op <- loadOperand8 inst
            a  <- load8 A
            let carry = b2W8 $ getFlag FC sr
            bcd <- (\model -> getFlag FD sr && (model /= NES_2A03)) <$> getModel
            let (r, ncarry) = case bcd of
                                  False -> let res = a + op + carry
                                            in (res, if carry == 1 then res <= a else res < a)
                                  -- http://forum.6502.org/viewtopic.php?p=13441
                                  True -> let n0     = carry + (a .&. 0x0F) + (op .&. 0x0F)
                                              hcarry = n0 >= 10
                                              n1     = b2W8 hcarry + (a `shiftR` 4) + (op `shiftR` 4)
                                              n0'    = if hcarry then n0 - 10 else n0
                                              carry' = n1 >= 10
                                              n1'    = if ncarry then n1 - 10 else n1
                                           in ((n1' `shiftL` 4) + n0', carry')
            store8Trace A r
            -- http://forums.nesdev.com/viewtopic.php?p=60520
            let overflow = (a `xor` r) .&. (op `xor` r) .&. 0x80 /= 0
            store8Trace SR . modifyFlag FC ncarry . modifyFlag FV overflow . setNZ r $ sr
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
            sr <- load8 SR
            op <- loadOperand8 inst
            a  <- load8 A
            let carry = b2W8 . not $ getFlag FC sr
            bcd <- (\model -> getFlag FD sr && (model /= NES_2A03)) <$> getModel
            let (r, ncarry) = case bcd of
                                  False -> let res = a - (op + carry) :: Word8
                                            in (res, not $ if carry == 1 then res >= a else res > a)
                                  -- http://forum.6502.org/viewtopic.php?p=13441
                                  True -> let ix     = fromIntegral op :: Int
                                              xdec   = ((ix `shiftR` 4) * 10) + (ix .&. 0x0F)
                                                       + fromIntegral carry
                                              ia     = fromIntegral a :: Int
                                              adec   = ((ia `shiftR` 4) * 10) + (ia .&. 0x0F) - xdec
                                              carry' = adec < 0
                                              adec'  = if carry' then adec + 100 else adec
                                              res    = ((adec' `div` 10) `shiftL` 4) + (adec' `mod` 10)
                                           in (fromIntegral res :: Word8, not carry')
            store8Trace A r
            -- http://forums.nesdev.com/viewtopic.php?p=60520
            let overflow = (a `xor` r) .&. ((complement op) `xor` r) .&. 0x80 /= 0
            store8Trace SR . modifyFlag FC ncarry . modifyFlag FV overflow . setNZ r $ sr
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
            op <- loadOperand8 inst
            let m = op + 1
            storeOperand8 inst m
            sr <- load8 SR
            a  <- load8 A
            let carry = b2W8 . not $ getFlag FC sr
            bcd <- (\model -> getFlag FD sr && (model /= NES_2A03)) <$> getModel
            let (r, ncarry) = case bcd of
                                  False -> let res = a - (m + carry) :: Word8
                                            in (res, not $ if carry == 1 then res >= a else res > a)
                                  -- http://forum.6502.org/viewtopic.php?p=13441
                                  True -> let ix     = fromIntegral m :: Int
                                              xdec   = ((ix `shiftR` 4) * 10) + (ix .&. 0x0F)
                                                       + fromIntegral carry
                                              ia     = fromIntegral a :: Int
                                              adec   = ((ia `shiftR` 4) * 10) + (ia .&. 0x0F) - xdec
                                              carry' = adec < 0
                                              adec'  = if carry' then adec + 100 else adec
                                              res    = ((adec' `div` 10) `shiftL` 4) + (adec' `mod` 10)
                                           in (fromIntegral res :: Word8, not carry')
            store8Trace A r
            -- http://forums.nesdev.com/viewtopic.php?p=60520
            let overflow = (a `xor` r) .&. ((complement m) `xor` r) .&. 0x80 /= 0
            store8Trace SR . modifyFlag FC ncarry . modifyFlag FV overflow . setNZ r $ sr
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
            rorcarry <- getFlag FC <$> load8 SR
            let ror = (op `shiftR` 1) .|. if rorcarry then 128 else 0
            storeOperand8 inst ror
            sr <- load8 SR
            a  <- load8 A
            let carry = b2W8 $ testBit op 0
            bcd <- (\model -> getFlag FD sr && (model /= NES_2A03)) <$> getModel
            let (r, ncarry) = case bcd of
                                  False -> let res = a + ror + carry
                                            in (res, if carry == 1 then res <= a else res < a)
                                  -- http://forum.6502.org/viewtopic.php?p=13441
                                  True -> let n0     = carry + (a .&. 0x0F) + (ror .&. 0x0F)
                                              hcarry = n0 >= 10
                                              n1     = b2W8 hcarry + (a `shiftR` 4) + (ror `shiftR` 4)
                                              n0'    = if hcarry then n0 - 10 else n0
                                              carry' = n1 >= 10
                                              n1'    = if ncarry then n1 - 10 else n1
                                           in ((n1' `shiftL` 4) + n0', carry')
            store8Trace A r
            -- http://forums.nesdev.com/viewtopic.php?p=60520
            let overflow = (a `xor` r) .&. (ror `xor` r) .&. 0x80 /= 0
            store8Trace SR . modifyFlag FC ncarry . modifyFlag FV overflow . setNZ r $ sr
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


{-
; ANC - Sets SR based on A AND M
;
; Does A AND M, setting N and Z flags based on the result. Then it copies N
; (bit 7) to C. A and M are not modified.
;
;    A AND M                          N Z C I D V
;                                     + + + - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
; C Carry Flag        Set if bit 7 set
;
;SYNTAX        MODE         HEX LEN TIM
;--------------------------------------
;ANC($0B) #$00 Immediate    $0B  2   2
DCB #$0B
DCB #$00
;ANC($2B) #$00 Immediate    $2B  2   2
DCB #$2B
DCB #$00

 0B *ANC imm 2   $0B: bytes: 2 cycles: 2 A____=>____P __ 
 2B *ANC imm 2   $2B: bytes: 2 cycles: 2 A____=>____P __ 
   ANC #i ($0B ii, $2B ii; 2 cycles) 
   Does AND #i, setting N and Z flags based on the result. Then it copies N (bit 7) to C.
   A:=A&#{imm} NZC
   this command performs an AND operation only, but bit 7 is put into the carry, as if the ASL/ROL would have been executed.
   AND byte with accumulator. If result is negative then carry is
   set. Status flags: N,Z,C
   -
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Immediate   |ANC #arg   |$0B| 2 | 2
  Immediate   |ANC #arg   |$2B| 2 | 2
-}
        ANC -> do
            let baseC = 2 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            a  <- load8 A
            sr <- load8 SR
            let r = op .&. a
            store8Trace SR . modifyFlag FC (testBit r 7) . setNZ r $ sr
            update16 PC (ilen +)
            advCycles baseC

{-
; ALR - Combined AND + LSR
;
; Does A AND M followed by LSR A.
;
;    A = A AND M                      N Z C I D V
;    0 -> [76543210] -> C             0 + + - - -
;
; C Carry Flag        Set to contents of old bit 0
; Z Zero Flag         Set if result = 0
; N Negative Flag     Set to 0
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;ALR #$00     Immediate     $4B  2   2
DCB #$4B
DCB #$00

 4B *ALR imm 2   $4B: bytes: 2 cycles: 2 A____=>A___P __ 
   ALR #i ($4B ii; 2 cycles) 
   Equivalent to AND #i then LSR A.
   A:=(A&#{imm})/2 NZC
   AND byte with accumulator, then shift right one bit in accumu-
   lator. Status flags: N,Z,C
   -   
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Immediate   |ALR #arg   |$4B| 2 | 2
-}
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

{-
; ARR - Combined AND + ROR with different SR effects
;
; Similar to doing AND Immediate followed by ROR A, but setting the flags
; C and V differently.
;
; In decimal mode, this instruction has some rather strange behavior,
; explained in detail in http://www.viceteam.org/plain/64doc.txt
;
;  A = ROR (A AND M)                  N Z C I D V
;                                     + + + - - +
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of the result is set
; C Carry Flag        Set if bit 6 of the result is set
; V Overflow Flag     Set to Bit5 ^ Bit6
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;ARR #$00     Immediate     $6B  2   2
DCB #$6B
DCB #$00

 6B *ARR imm 2   $6B: bytes: 2 cycles: 2 A___P=>A___P __ 
   ARR #i ($6B ii; 2 cycles)
   Similar to AND #i then ROR A, except sets the flags differently.
   N and Z are normal, but C is bit 6 and V is bit 6 xor bit 5.
   A:=(A&#{imm})/2 NVZC
   part of this command are some ADC mechanisms. following effects appear after AND but before ROR: the V-Flag is set according to (A and #{imm})+#{imm}, bit 0 does NOT go into carry, but bit 7 is exchanged with the carry.
   AND byte with accumulator, then rotate one bit right in accu-
   mulator and check bit 5 and 6:
   If both bits are 1: set C, clear V.
   If both bits are 0: clear C and V.
   If only bit 5 is 1: set V, clear C.
   If only bit 6 is 1: set C and V.
   Status flags: N,V,Z,C
   -
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Immediate   |ARR #arg   |$6B| 2 | 2
-}
        ARR -> do
            -- TODO: This instruction should behave very different in decimal mode
            let baseC = 2 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            op <- loadOperand8 inst
            sr <- load8 SR
            a  <- load8 A
            let and'   = op .&. a
                carry  = getFlag FC sr
                r      = (and' `shiftR` 1) .|. if carry then 128 else 0
                ncarry = testBit and' 0
                over   = (testBit r 5) /= (testBit r 6) -- Bit5 ^ Bit6
            store8Trace SR . modifyFlag FV over . setNZC r ncarry $ sr
            store8Trace A r
            update16 PC (ilen +)
            advCycles baseC

{-
; XAA - Unstable opcode, like a three register AND
;
; See http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
; for a good analysis. This opcode is not stable on a real 6502 and its
; result depends on analog effects and varies between different CPUs and
; operating conditions.
;
;  A = (A OR magic) AND X AND M       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of A is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;XAA #$00     Immediate     $8B  2   2  *Highly Unstable*
DCB #$8B
DCB #$00

 8B *XAA imm 2   $8B: bytes: 2 cycles: 2 _____=>A___P __ 
   http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
   XAA - will more or less AND together the three inputs: the X register, the A register, and the immediate operand.
   A = (A | magic) & X & imm
    N and Z flags are set according to the result of XAA 
-}
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


{-
; AHX - Store A & X & (ADDR_HI + 1) into M
;
; There are some conflicting description of what this opcode does, but this
; version seems to match the reference best.
;
;  A & X & (ADDR_HI + 1) -> M         N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;AHX ($00),Y  IndIdx        $93  2   6
DCB #$93
DCB #$00
;AHX $0000,Y  Absolute,Y    $9F  3   5
DCB #$9F
DCB #$00
DCB #$00

 93 *AHX izy 6   $93: bytes: 2 cycles: 6 _____=>_____ RW izy
 9F *AHX aby 5   $9F: bytes: 3 cycles: 5 _____=>_____ RW absy
   {adr}:=A&X&H NoFlags unstable in certain matters
  SHA $93,$9F     Store (A & X & (ADDR_HI + 1))
   AND X register with accumulator then AND result with 7 and
   store in memory. Status flags: -
   -
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   (Indirect),Y|AXA arg    |$93| 2 | 6
   Absolute,Y  |AXA arg,Y  |$9F| 3 | 5
-}
        AHX -> do
            let baseC = 1 + getAMCycles am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            x <- load8 X
            addrHI <- \case Addr addr -> return . snd . splitW16 $ addr
                            _         -> trace "AM Err" >> return 0
                      =<< getOperandAddr8 inst
            let r = a .&. x .&. (addrHI + 1);
            storeOperand8 inst r
            update16 PC (ilen +)
            advCycles baseC


{-
; TAS - AND A, X, SP, ADDR_HI
;
; AND X register with accumulator and store result in stack pointer, then AND
; stack pointer with the high byte of the target address of the argument + 1.
; Store result in memory.
;
;  X AND A -> SP                      N Z C I D V
;  SP AND (ADDR_HI + 1) -> M          - - - - - -
;
;SYNTAX      MODE           HEX LEN TIM
;--------------------------------------
;TAS $0000,Y Absolute,Y     $9B  3   5
DCB #$9B
DCB #$00
DCB #$00

 9B *TAS aby 5   $9B: bytes: X cycles: 5 __Y__=>___S_ _W 
   S:=A&X {adr}:=S&H NoFlags unstable in certain matters
   AND X register with accumulator and store result in stack
   pointer, then AND stack pointer with the high byte of the
   target address of the argument + 1. Store result in memory.
   -   
   S = X AND A, M = S AND HIGH(arg) + 1
   - 
   Status flags: -
   -   
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Absolute,Y  |XAS arg,Y  |$9B| 3 | 5
-}
        TAS -> do
            let baseC = 1 + getAMCycles am
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            a <- load8 A
            x <- load8 X
            let sp = x .&. a
            store8Trace SP sp
            addrHI <- \case Addr addr -> return . snd . splitW16 $ addr
                            _         -> trace "AM Err" >> return 0
                      =<< getOperandAddr8 inst
            storeOperand8 inst $ sp .&. (addrHI + 1)
            update16 PC (ilen +)
            advCycles baseC


{-
; SHX - X AND ADDR_HI
;
; AND X register with the high byte of the target address of the argument + 1.
; Store the result in memory.
;
;  X AND (ADDR_HI + 1) -> M           N Z C I D V
;                                     - - - - - -
;
;SYNTAX      MODE           HEX LEN TIM
;--------------------------------------
;SHX $0000,Y Absolute,Y     $9E  3   5
DCB #$9E
DCB #$00
DCB #$00

 9E *SHX aby 5   $9E: bytes: 3 cycles: 5 _X___=>_____ RW absy
   {adr}:=X&H NoFlags unstable in certain matters
   AND X register with the high byte of the target address of the
   argument + 1. Store the result in memory.
   -
   M = X AND HIGH(arg) + 1
   -
   Status flags: -
   -
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Absolute,Y  |SXA arg,Y  |$9E| 3 | 5
-}
        SHX -> do
            let baseC = 1 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            x <- load8 X
            addrHI <- \case Addr addr -> return . snd . splitW16 $ addr
                            _         -> trace "AM Err" >> return 0
                      =<< getOperandAddr8 inst
            storeOperand8 inst $ x .&. (addrHI + 1)
            update16 PC (ilen +)
            advCycles baseC


{-
; SHY - Y AND ADDR_HI
;
; AND Y register with the high byte of the target address of the argument + 1.
; Store the result in memory.
;
;  Y AND (ADDR_HI + 1) -> M           N Z C I D V
;                                     - - - - - -
;
;SYNTAX      MODE           HEX LEN TIM
;--------------------------------------
;SHY $0000,Y Absolute,Y     $9C  3   5
DCB #$9C
DCB #$00
DCB #$00

 9C *SHY abx 5   $9C: bytes: 3 cycles: 5 __Y__=>_____ RW absx
   {adr}:=Y&H NoFlags unstable in certain matters
   AND Y register with the high byte of the target address of the
   argument + 1. Store the result in memory.
   -
   M = Y AND HIGH(arg) + 1
   -
   Status flags: -
   -
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Absolute,X  |SHY arg,X  |$9C| 3 | 5
-}
        SHY -> do
            let baseC = 1 :: Word64
            trace $ printf "%02X:%-11s I%ib%iC   " w (show inst) ilen baseC
            traceNoOpLoad
            y <- load8 Y
            addrHI <- \case Addr addr -> return . snd . splitW16 $ addr
                            _         -> trace "AM Err" >> return 0
                      =<< getOperandAddr8 inst
            storeOperand8 inst $ y .&. (addrHI + 1)
            update16 PC (ilen +)
            advCycles baseC


{-
; LAS - Load A, X and SP with SP AND M
;
; AND memory with stack pointer, transfer result to accumulator, X register
; and stack pointer.
;
;    SP AND M -> A, X, SP             N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;LAS $0000,Y  Absolute,Y    $BB  3   4+
DCB #$BB
DCB #$00
DCB #$00

 BB *LAS aby 4*  $BB: bytes: 3 cycles: 4 ___S_=>AX_SP R_ absy
   A,X,S:={adr}&S NZ
   AND memory with stack pointer, transfer result to accumulator,
   X register and stack pointer.
   Status flags: N,Z
   -
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Absolute,Y  |LAR arg,Y  |$BB| 3 | 4 * add one cycle when page boundary is crossed
-}
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

{-
; AXS - Store A AND X minus M into X
;
; Stores to X the value of (A & X) - Immediate. This instruction does not have
; any decimal mode, and it does not affect the V flag. Also Carry will be
; ignored in the subtraction, but set according to the result (like CMP).
;
;  X <- (A & X) - M                   N Z C I D V
;                                     + + + - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
; C Carry Flag        Set if (A & X) >= M
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;AXS #$00     Immediate     $CB  2   2
DCB #$CB
DCB #$00

 CB *AXS imm 2   $CB: bytes: 2 cycles: 2 _____=>_X__P __ 
 AXS #i ($CB ii, 2 cycles)
    Sets X to {(A AND X) - #value without borrow}, and updates NZC. One might use TXA AXS #-element_size to iterate through an array of structures or other elements larger than a byte, where the 6502 architecture usually prefers a structure of arrays. For example, TXA AXS #$FC could step to the next OAM entry or to the next APU channel, saving one byte and four cycles over four INXs. Also called SBX. 
    X:=A&X-#{imm} NZC
    performs CMP and DEX at the same time, so that the MINUS sets the flag like CMP, not SBC.
   AND X register with accumulator and store result in X regis-
   ter, then subtract byte from X register (without borrow).
   Status flags: N,Z,C
   -
   Addressing  |Mnemonics  |Opc|Sz | n
   ------------|-----------|---|---|---
   Immediate   |AXS #arg   |$CB| 2 | 2
-}
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

