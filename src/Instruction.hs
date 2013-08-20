
module Instruction ( AddressMode(..)
                   , operandLen
                   , Mnemonic(..)
                   , OpCode(..)
                   , decodeOpCode
                   , Instruction(..)
                   , instructionLen
                   , decodeInstruction
                   , decodeInstructionM
                   ) where

-- This module contains types and function for representing and decoding
-- all official instructions of the 6502

import Util (makeW16)
import MonadEmulator (MonadEmulator(..), LoadStore(..))

import Data.Word (Word8)
import Text.Printf
import Control.Applicative ((<$>))
import qualified Data.Vector.Unboxed as VU

data AddressMode =
      Implied
    | Accumulator
    | Immediate
    | ZeroPage
    | ZeroPageX
    | ZeroPageY
    | Relative
    | Absolute
    | AbsoluteX
    | AbsoluteY
    | Indirect
    | IdxInd
    | IndIdx
      deriving (Show, Eq)

{-# INLINE operandLen #-}
operandLen :: AddressMode -> Int
operandLen Implied     = 0
operandLen Accumulator = 0
operandLen Immediate   = 1
operandLen ZeroPage    = 1
operandLen ZeroPageX   = 1
operandLen ZeroPageY   = 1
operandLen Relative    = 1
operandLen Absolute    = 2
operandLen AbsoluteX   = 2
operandLen AbsoluteY   = 2
operandLen Indirect    = 2
operandLen IdxInd      = 1
operandLen IndIdx      = 1

data Mnemonic =
    -- Official
      ADC | AND | ASL | BCC | BCS | BEQ
    | BIT | BMI | BNE | BPL | BRK | BVC
    | BVS | CLC | CLD | CLI | CLV | CMP
    | CPX | CPY | DEC | DEX | DEY | EOR
    | INC | INX | INY | JMP | JSR | LDA
    | LDX | LDY | LSR | NOP | ORA | PHA
    | PHP | PLA | PLP | ROL | ROR | RTI
    | RTS | SBC | SEC | SED | SEI | STA
    | STX | STY | TAX | TAY | TSX | TXA
    | TXS | TYA
    -- Illegal / Unofficial
    | DCB | KIL | LAX | SAX
      deriving (Show, Eq)

-- We store the binary representation as well so we can later distinguish otherwise indentical instructions
data OpCode = OpCode Word8 Mnemonic AddressMode

decodeOpCode :: Word8 -> OpCode
decodeOpCode w = case w of
    -- Official
    ; 0x69 -> OpCode w ADC Immediate   ; 0x65 -> OpCode w ADC ZeroPage    ; 0x75 -> OpCode w ADC ZeroPageX
    ; 0x6D -> OpCode w ADC Absolute    ; 0x7D -> OpCode w ADC AbsoluteX   ; 0x79 -> OpCode w ADC AbsoluteY
    ; 0x61 -> OpCode w ADC IdxInd      ; 0x71 -> OpCode w ADC IndIdx      ; 0x29 -> OpCode w AND Immediate
    ; 0x25 -> OpCode w AND ZeroPage    ; 0x35 -> OpCode w AND ZeroPageX   ; 0x2D -> OpCode w AND Absolute
    ; 0x3D -> OpCode w AND AbsoluteX   ; 0x39 -> OpCode w AND AbsoluteY   ; 0x21 -> OpCode w AND IdxInd
    ; 0x31 -> OpCode w AND IndIdx      ; 0x0A -> OpCode w ASL Accumulator ; 0x06 -> OpCode w ASL ZeroPage
    ; 0x16 -> OpCode w ASL ZeroPageX   ; 0x0E -> OpCode w ASL Absolute    ; 0x1E -> OpCode w ASL AbsoluteX
    ; 0x90 -> OpCode w BCC Relative    ; 0xB0 -> OpCode w BCS Relative    ; 0xF0 -> OpCode w BEQ Relative
    ; 0x24 -> OpCode w BIT ZeroPage    ; 0x2C -> OpCode w BIT Absolute    ; 0x30 -> OpCode w BMI Relative
    ; 0xD0 -> OpCode w BNE Relative    ; 0x10 -> OpCode w BPL Relative    ; 0x00 -> OpCode w BRK Implied
    ; 0x50 -> OpCode w BVC Relative    ; 0x70 -> OpCode w BVS Relative    ; 0x18 -> OpCode w CLC Implied
    ; 0xD8 -> OpCode w CLD Implied     ; 0x58 -> OpCode w CLI Implied     ; 0xB8 -> OpCode w CLV Implied
    ; 0xC9 -> OpCode w CMP Immediate   ; 0xC5 -> OpCode w CMP ZeroPage    ; 0xD5 -> OpCode w CMP ZeroPageX
    ; 0xCD -> OpCode w CMP Absolute    ; 0xDD -> OpCode w CMP AbsoluteX   ; 0xD9 -> OpCode w CMP AbsoluteY
    ; 0xC1 -> OpCode w CMP IdxInd      ; 0xD1 -> OpCode w CMP IndIdx      ; 0xE0 -> OpCode w CPX Immediate
    ; 0xE4 -> OpCode w CPX ZeroPage    ; 0xEC -> OpCode w CPX Absolute    ; 0xC0 -> OpCode w CPY Immediate
    ; 0xC4 -> OpCode w CPY ZeroPage    ; 0xCC -> OpCode w CPY Absolute    ; 0xC6 -> OpCode w DEC ZeroPage
    ; 0xD6 -> OpCode w DEC ZeroPageX   ; 0xCE -> OpCode w DEC Absolute    ; 0xDE -> OpCode w DEC AbsoluteX
    ; 0xCA -> OpCode w DEX Implied     ; 0x88 -> OpCode w DEY Implied     ; 0x49 -> OpCode w EOR Immediate
    ; 0x45 -> OpCode w EOR ZeroPage    ; 0x55 -> OpCode w EOR ZeroPageX   ; 0x4D -> OpCode w EOR Absolute
    ; 0x5D -> OpCode w EOR AbsoluteX   ; 0x59 -> OpCode w EOR AbsoluteY   ; 0x41 -> OpCode w EOR IdxInd
    ; 0x51 -> OpCode w EOR IndIdx      ; 0xE6 -> OpCode w INC ZeroPage    ; 0xF6 -> OpCode w INC ZeroPageX
    ; 0xEE -> OpCode w INC Absolute    ; 0xFE -> OpCode w INC AbsoluteX   ; 0xE8 -> OpCode w INX Implied
    ; 0xC8 -> OpCode w INY Implied     ; 0x4C -> OpCode w JMP Absolute    ; 0x6C -> OpCode w JMP Indirect
    ; 0x20 -> OpCode w JSR Absolute    ; 0xA9 -> OpCode w LDA Immediate   ; 0xA5 -> OpCode w LDA ZeroPage
    ; 0xB5 -> OpCode w LDA ZeroPageX   ; 0xAD -> OpCode w LDA Absolute    ; 0xBD -> OpCode w LDA AbsoluteX
    ; 0xB9 -> OpCode w LDA AbsoluteY   ; 0xA1 -> OpCode w LDA IdxInd      ; 0xB1 -> OpCode w LDA IndIdx
    ; 0xA2 -> OpCode w LDX Immediate   ; 0xA6 -> OpCode w LDX ZeroPage    ; 0xB6 -> OpCode w LDX ZeroPageY
    ; 0xAE -> OpCode w LDX Absolute    ; 0xBE -> OpCode w LDX AbsoluteY   ; 0xA0 -> OpCode w LDY Immediate
    ; 0xA4 -> OpCode w LDY ZeroPage    ; 0xB4 -> OpCode w LDY ZeroPageX   ; 0xAC -> OpCode w LDY Absolute
    ; 0xBC -> OpCode w LDY AbsoluteX   ; 0x4A -> OpCode w LSR Accumulator ; 0x46 -> OpCode w LSR ZeroPage
    ; 0x56 -> OpCode w LSR ZeroPageX   ; 0x4E -> OpCode w LSR Absolute    ; 0x5E -> OpCode w LSR AbsoluteX
    ; 0xEA -> OpCode w NOP Implied     ; 0x09 -> OpCode w ORA Immediate   ; 0x05 -> OpCode w ORA ZeroPage
    ; 0x15 -> OpCode w ORA ZeroPageX   ; 0x0D -> OpCode w ORA Absolute    ; 0x1D -> OpCode w ORA AbsoluteX
    ; 0x19 -> OpCode w ORA AbsoluteY   ; 0x01 -> OpCode w ORA IdxInd      ; 0x11 -> OpCode w ORA IndIdx
    ; 0x48 -> OpCode w PHA Implied     ; 0x08 -> OpCode w PHP Implied     ; 0x68 -> OpCode w PLA Implied
    ; 0x28 -> OpCode w PLP Implied     ; 0x2A -> OpCode w ROL Accumulator ; 0x26 -> OpCode w ROL ZeroPage
    ; 0x36 -> OpCode w ROL ZeroPageX   ; 0x2E -> OpCode w ROL Absolute    ; 0x3E -> OpCode w ROL AbsoluteX
    ; 0x6A -> OpCode w ROR Accumulator ; 0x66 -> OpCode w ROR ZeroPage    ; 0x76 -> OpCode w ROR ZeroPageX
    ; 0x6E -> OpCode w ROR Absolute    ; 0x7E -> OpCode w ROR AbsoluteX   ; 0x40 -> OpCode w RTI Implied
    ; 0x60 -> OpCode w RTS Implied     ; 0xE9 -> OpCode w SBC Immediate   ; 0xE5 -> OpCode w SBC ZeroPage
    ; 0xF5 -> OpCode w SBC ZeroPageX   ; 0xED -> OpCode w SBC Absolute    ; 0xFD -> OpCode w SBC AbsoluteX
    ; 0xF9 -> OpCode w SBC AbsoluteY   ; 0xE1 -> OpCode w SBC IdxInd      ; 0xF1 -> OpCode w SBC IndIdx
    ; 0x38 -> OpCode w SEC Implied     ; 0xF8 -> OpCode w SED Implied     ; 0x78 -> OpCode w SEI Implied
    ; 0x85 -> OpCode w STA ZeroPage    ; 0x95 -> OpCode w STA ZeroPageX   ; 0x8D -> OpCode w STA Absolute
    ; 0x9D -> OpCode w STA AbsoluteX   ; 0x99 -> OpCode w STA AbsoluteY   ; 0x81 -> OpCode w STA IdxInd
    ; 0x91 -> OpCode w STA IndIdx      ; 0x86 -> OpCode w STX ZeroPage    ; 0x96 -> OpCode w STX ZeroPageY
    ; 0x8E -> OpCode w STX Absolute    ; 0x84 -> OpCode w STY ZeroPage    ; 0x94 -> OpCode w STY ZeroPageX
    ; 0x8C -> OpCode w STY Absolute    ; 0xAA -> OpCode w TAX Implied     ; 0xA8 -> OpCode w TAY Implied
    ; 0xBA -> OpCode w TSX Implied     ; 0x8A -> OpCode w TXA Implied     ; 0x9A -> OpCode w TXS Implied
    ; 0x98 -> OpCode w TYA Implied
    -- Illegal / Unofficial
    ; 0x02 -> OpCode w KIL Implied     ; 0x12 -> OpCode w KIL Implied     ; 0x22 -> OpCode w KIL Implied
    ; 0x32 -> OpCode w KIL Implied     ; 0x42 -> OpCode w KIL Implied     ; 0x52 -> OpCode w KIL Implied
    ; 0x62 -> OpCode w KIL Implied     ; 0x72 -> OpCode w KIL Implied     ; 0x92 -> OpCode w KIL Implied
    ; 0xB2 -> OpCode w KIL Implied     ; 0xD2 -> OpCode w KIL Implied     ; 0xF2 -> OpCode w KIL Implied
    ; 0x7A -> OpCode w NOP Implied     ; 0x5A -> OpCode w NOP Implied     ; 0x1A -> OpCode w NOP Implied
    ; 0x3A -> OpCode w NOP Implied     ; 0xDA -> OpCode w NOP Implied     ; 0xFA -> OpCode w NOP Implied
    ; 0x80 -> OpCode w NOP Immediate   ; 0x82 -> OpCode w NOP Immediate   ; 0x89 -> OpCode w NOP Immediate
    ; 0xC2 -> OpCode w NOP Immediate   ; 0xE2 -> OpCode w NOP Immediate   ; 0x04 -> OpCode w NOP ZeroPage
    ; 0x64 -> OpCode w NOP ZeroPage    ; 0x44 -> OpCode w NOP ZeroPage    ; 0x0C -> OpCode w NOP Absolute
    ; 0x14 -> OpCode w NOP ZeroPageX   ; 0x34 -> OpCode w NOP ZeroPageX   ; 0x54 -> OpCode w NOP ZeroPageX
    ; 0x74 -> OpCode w NOP ZeroPageX   ; 0xD4 -> OpCode w NOP ZeroPageX   ; 0xF4 -> OpCode w NOP ZeroPageX
    ; 0x1C -> OpCode w NOP AbsoluteX   ; 0x3C -> OpCode w NOP AbsoluteX   ; 0x5C -> OpCode w NOP AbsoluteX
    ; 0x7C -> OpCode w NOP AbsoluteX   ; 0xDC -> OpCode w NOP AbsoluteX   ; 0xFC -> OpCode w NOP AbsoluteX
    ; 0xAB -> OpCode w LAX Immediate   ; 0xA7 -> OpCode w LAX ZeroPage    ; 0xB7 -> OpCode w LAX ZeroPageY
    ; 0xAF -> OpCode w LAX Absolute    ; 0xBF -> OpCode w LAX AbsoluteY   ; 0xA3 -> OpCode w LAX IdxInd
    ; 0xB3 -> OpCode w LAX IndIdx      ; 0x87 -> OpCode w SAX ZeroPage    ; 0x97 -> OpCode w SAX ZeroPageY
    ; 0x8F -> OpCode w SAX Absolute    ; 0x83 -> OpCode w SAX IdxInd      ; 0xEB -> OpCode w SBC Immediate
    ; _    -> OpCode w DCB Implied

data Instruction = Instruction OpCode [Word8]

showAMAndOP :: AddressMode -> [Word8] -> String
showAMAndOP am op =
    case am of
        Implied     -> case op of []           ->        ""                             ; _ -> "OpLnErr"
        Accumulator -> case op of []           ->        " A"                           ; _ -> "OpLnErr"
        Immediate   -> case op of [opl]        -> printf " #$%02X"              opl     ; _ -> "OpLnErr"
        ZeroPage    -> case op of [opl]        -> printf " $%02X"               opl     ; _ -> "OpLnErr"
        ZeroPageX   -> case op of [opl]        -> printf " $%02X,X"             opl     ; _ -> "OpLnErr"
        ZeroPageY   -> case op of [opl]        -> printf " $%02X,Y"             opl     ; _ -> "OpLnErr"
        Relative    -> case op of [opl]        -> printf " $%02X"               opl     ; _ -> "OpLnErr"
        Absolute    -> case op of (opl:oph:[]) -> printf " $%04X"     $ makeW16 opl oph ; _ -> "OpLnErr"
        AbsoluteX   -> case op of (opl:oph:[]) -> printf " $%04X,X"   $ makeW16 opl oph ; _ -> "OpLnErr"
        AbsoluteY   -> case op of (opl:oph:[]) -> printf " $%04X,Y"   $ makeW16 opl oph ; _ -> "OpLnErr"
        Indirect    -> case op of (opl:oph:[]) -> printf " ($%04X)"   $ makeW16 opl oph ; _ -> "OpLnErr"
        IdxInd      -> case op of [opl]        -> printf " ($%02X,X)"           opl     ; _ -> "OpLnErr"
        IndIdx      -> case op of [opl]        -> printf " ($%02X),Y"           opl     ; _ -> "OpLnErr"

-- Many illegal opcodes are identical in behavior and addressing mode, we need
-- to look at the actual binary encoding if we want to distinguish them. For
-- mnemonics where legal and illegal variants exist, only flag the illegal ones
-- as ambiguos so we don't clutter up the standard opcode disassembly
isAmbiguousMn :: Word8 -> Mnemonic -> Bool
isAmbiguousMn w8 mn = case mn of
    DCB -> True
    KIL -> True
    NOP -> (w8 /= 0xEA) -- Only a single official variant
    SBC -> (w8 == 0xEB) -- All except one official
    _   -> False

instance Show Instruction where
    show (Instruction (OpCode w mn am) op) =
        show mn ++ (if isAmbiguousMn w mn then printf "($%02X)" w else "") ++ showAMAndOP am op

{-# INLINE instructionLen #-}
instructionLen :: Instruction -> Int
instructionLen (Instruction (OpCode _ _ a) _) = 1 + operandLen a

decodeInstruction :: VU.Vector Word8 -> Int -> Maybe Instruction
decodeInstruction mem pc = do
    opMem <- mem VU.!? pc
    let opc@(OpCode _ _ am) = decodeOpCode opMem
    case operandLen am of
        -- TODO: We should probably just return a DCB in case the operands are missing
        1 -> do op1 <- mem VU.!? (pc + 1)
                return $ Instruction opc [op1]
        2 -> do op1 <- mem VU.!? (pc + 1)
                op2 <- mem VU.!? (pc + 2)
                return $ Instruction opc [op1, op2]
        _ ->    return $ Instruction opc []

{-# INLINE decodeInstructionM #-}
decodeInstructionM :: MonadEmulator m => m Instruction
decodeInstructionM = do
    pc <- load16 PC
    opc@(OpCode _ _ am) <- decodeOpCode <$> (load8 $ Addr pc)
    Instruction opc <$> case operandLen am of
            1 -> mapM (load8 . Addr) [ pc + 1         ]
            2 -> mapM (load8 . Addr) [ pc + 1, pc + 2 ]
            _ -> return              [                ]

