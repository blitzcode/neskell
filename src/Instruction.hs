
{-# LANGUAGE ViewPatterns #-}

module Instruction ( AddressMode(..)
                   , operandLen
                   , Mnemonic(..)
                   , OpCodeView(..)
                   , OpCode
                   , viewOpCode
                   , decodeOpCode
                   , Instruction(..)
                   , instructionLen
                   , decodeInstructionM
                   , disassemble
                   ) where

-- This module contains types and function for representing and decoding all instructions of the 6502

import Util (makeW16)
import MonadEmulator (MonadEmulator(..), LoadStore(..))

import Data.Word (Word8)
import Text.Printf
import Control.Applicative ((<$>))
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

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
    | KIL | LAX | SAX | DCP | ISC | RLA
    | RRA | SLO | SRE | ANC | ALR | ARR
    | XAA | AHX | TAS | SHX | SHY | LAS
    | AXS
      deriving (Show, Eq)

-- We store the binary representation as well so we can later distinguish
-- otherwise identical instructions. The actual OpCode type is abstract, use
-- view patterns to access (http://stackoverflow.com/a/8172768/1898360)
data OpCodeView = OpCode Word8 Mnemonic AddressMode
newtype OpCode = OpCodeC { viewOpCode :: OpCodeView }

decodeOpCode :: Word8 -> OpCode
decodeOpCode w = let o = OpCode w in OpCodeC $ case w of
    -- Official
    ; 0x69 -> o ADC Immediate   ; 0x65 -> o ADC ZeroPage    ; 0x75 -> o ADC ZeroPageX
    ; 0x6D -> o ADC Absolute    ; 0x7D -> o ADC AbsoluteX   ; 0x79 -> o ADC AbsoluteY
    ; 0x61 -> o ADC IdxInd      ; 0x71 -> o ADC IndIdx      ; 0x29 -> o AND Immediate
    ; 0x25 -> o AND ZeroPage    ; 0x35 -> o AND ZeroPageX   ; 0x2D -> o AND Absolute
    ; 0x3D -> o AND AbsoluteX   ; 0x39 -> o AND AbsoluteY   ; 0x21 -> o AND IdxInd
    ; 0x31 -> o AND IndIdx      ; 0x0A -> o ASL Accumulator ; 0x06 -> o ASL ZeroPage
    ; 0x16 -> o ASL ZeroPageX   ; 0x0E -> o ASL Absolute    ; 0x1E -> o ASL AbsoluteX
    ; 0x90 -> o BCC Relative    ; 0xB0 -> o BCS Relative    ; 0xF0 -> o BEQ Relative
    ; 0x24 -> o BIT ZeroPage    ; 0x2C -> o BIT Absolute    ; 0x30 -> o BMI Relative
    ; 0xD0 -> o BNE Relative    ; 0x10 -> o BPL Relative    ; 0x00 -> o BRK Implied
    ; 0x50 -> o BVC Relative    ; 0x70 -> o BVS Relative    ; 0x18 -> o CLC Implied
    ; 0xD8 -> o CLD Implied     ; 0x58 -> o CLI Implied     ; 0xB8 -> o CLV Implied
    ; 0xC9 -> o CMP Immediate   ; 0xC5 -> o CMP ZeroPage    ; 0xD5 -> o CMP ZeroPageX
    ; 0xCD -> o CMP Absolute    ; 0xDD -> o CMP AbsoluteX   ; 0xD9 -> o CMP AbsoluteY
    ; 0xC1 -> o CMP IdxInd      ; 0xD1 -> o CMP IndIdx      ; 0xE0 -> o CPX Immediate
    ; 0xE4 -> o CPX ZeroPage    ; 0xEC -> o CPX Absolute    ; 0xC0 -> o CPY Immediate
    ; 0xC4 -> o CPY ZeroPage    ; 0xCC -> o CPY Absolute    ; 0xC6 -> o DEC ZeroPage
    ; 0xD6 -> o DEC ZeroPageX   ; 0xCE -> o DEC Absolute    ; 0xDE -> o DEC AbsoluteX
    ; 0xCA -> o DEX Implied     ; 0x88 -> o DEY Implied     ; 0x49 -> o EOR Immediate
    ; 0x45 -> o EOR ZeroPage    ; 0x55 -> o EOR ZeroPageX   ; 0x4D -> o EOR Absolute
    ; 0x5D -> o EOR AbsoluteX   ; 0x59 -> o EOR AbsoluteY   ; 0x41 -> o EOR IdxInd
    ; 0x51 -> o EOR IndIdx      ; 0xE6 -> o INC ZeroPage    ; 0xF6 -> o INC ZeroPageX
    ; 0xEE -> o INC Absolute    ; 0xFE -> o INC AbsoluteX   ; 0xE8 -> o INX Implied
    ; 0xC8 -> o INY Implied     ; 0x4C -> o JMP Absolute    ; 0x6C -> o JMP Indirect
    ; 0x20 -> o JSR Absolute    ; 0xA9 -> o LDA Immediate   ; 0xA5 -> o LDA ZeroPage
    ; 0xB5 -> o LDA ZeroPageX   ; 0xAD -> o LDA Absolute    ; 0xBD -> o LDA AbsoluteX
    ; 0xB9 -> o LDA AbsoluteY   ; 0xA1 -> o LDA IdxInd      ; 0xB1 -> o LDA IndIdx
    ; 0xA2 -> o LDX Immediate   ; 0xA6 -> o LDX ZeroPage    ; 0xB6 -> o LDX ZeroPageY
    ; 0xAE -> o LDX Absolute    ; 0xBE -> o LDX AbsoluteY   ; 0xA0 -> o LDY Immediate
    ; 0xA4 -> o LDY ZeroPage    ; 0xB4 -> o LDY ZeroPageX   ; 0xAC -> o LDY Absolute
    ; 0xBC -> o LDY AbsoluteX   ; 0x4A -> o LSR Accumulator ; 0x46 -> o LSR ZeroPage
    ; 0x56 -> o LSR ZeroPageX   ; 0x4E -> o LSR Absolute    ; 0x5E -> o LSR AbsoluteX
    ; 0xEA -> o NOP Implied     ; 0x09 -> o ORA Immediate   ; 0x05 -> o ORA ZeroPage
    ; 0x15 -> o ORA ZeroPageX   ; 0x0D -> o ORA Absolute    ; 0x1D -> o ORA AbsoluteX
    ; 0x19 -> o ORA AbsoluteY   ; 0x01 -> o ORA IdxInd      ; 0x11 -> o ORA IndIdx
    ; 0x48 -> o PHA Implied     ; 0x08 -> o PHP Implied     ; 0x68 -> o PLA Implied
    ; 0x28 -> o PLP Implied     ; 0x2A -> o ROL Accumulator ; 0x26 -> o ROL ZeroPage
    ; 0x36 -> o ROL ZeroPageX   ; 0x2E -> o ROL Absolute    ; 0x3E -> o ROL AbsoluteX
    ; 0x6A -> o ROR Accumulator ; 0x66 -> o ROR ZeroPage    ; 0x76 -> o ROR ZeroPageX
    ; 0x6E -> o ROR Absolute    ; 0x7E -> o ROR AbsoluteX   ; 0x40 -> o RTI Implied
    ; 0x60 -> o RTS Implied     ; 0xE9 -> o SBC Immediate   ; 0xE5 -> o SBC ZeroPage
    ; 0xF5 -> o SBC ZeroPageX   ; 0xED -> o SBC Absolute    ; 0xFD -> o SBC AbsoluteX
    ; 0xF9 -> o SBC AbsoluteY   ; 0xE1 -> o SBC IdxInd      ; 0xF1 -> o SBC IndIdx
    ; 0x38 -> o SEC Implied     ; 0xF8 -> o SED Implied     ; 0x78 -> o SEI Implied
    ; 0x85 -> o STA ZeroPage    ; 0x95 -> o STA ZeroPageX   ; 0x8D -> o STA Absolute
    ; 0x9D -> o STA AbsoluteX   ; 0x99 -> o STA AbsoluteY   ; 0x81 -> o STA IdxInd
    ; 0x91 -> o STA IndIdx      ; 0x86 -> o STX ZeroPage    ; 0x96 -> o STX ZeroPageY
    ; 0x8E -> o STX Absolute    ; 0x84 -> o STY ZeroPage    ; 0x94 -> o STY ZeroPageX
    ; 0x8C -> o STY Absolute    ; 0xAA -> o TAX Implied     ; 0xA8 -> o TAY Implied
    ; 0xBA -> o TSX Implied     ; 0x8A -> o TXA Implied     ; 0x9A -> o TXS Implied
    ; 0x98 -> o TYA Implied
    -- Illegal / Unofficial
    ; 0x02 -> o KIL Implied     ; 0x12 -> o KIL Implied     ; 0x22 -> o KIL Implied
    ; 0x32 -> o KIL Implied     ; 0x42 -> o KIL Implied     ; 0x52 -> o KIL Implied
    ; 0x62 -> o KIL Implied     ; 0x72 -> o KIL Implied     ; 0x92 -> o KIL Implied
    ; 0xB2 -> o KIL Implied     ; 0xD2 -> o KIL Implied     ; 0xF2 -> o KIL Implied
    ; 0x7A -> o NOP Implied     ; 0x5A -> o NOP Implied     ; 0x1A -> o NOP Implied
    ; 0x3A -> o NOP Implied     ; 0xDA -> o NOP Implied     ; 0xFA -> o NOP Implied
    ; 0x80 -> o NOP Immediate   ; 0x82 -> o NOP Immediate   ; 0x89 -> o NOP Immediate
    ; 0xC2 -> o NOP Immediate   ; 0xE2 -> o NOP Immediate   ; 0x04 -> o NOP ZeroPage
    ; 0x64 -> o NOP ZeroPage    ; 0x44 -> o NOP ZeroPage    ; 0x0C -> o NOP Absolute
    ; 0x14 -> o NOP ZeroPageX   ; 0x34 -> o NOP ZeroPageX   ; 0x54 -> o NOP ZeroPageX
    ; 0x74 -> o NOP ZeroPageX   ; 0xD4 -> o NOP ZeroPageX   ; 0xF4 -> o NOP ZeroPageX
    ; 0x1C -> o NOP AbsoluteX   ; 0x3C -> o NOP AbsoluteX   ; 0x5C -> o NOP AbsoluteX
    ; 0x7C -> o NOP AbsoluteX   ; 0xDC -> o NOP AbsoluteX   ; 0xFC -> o NOP AbsoluteX
    ; 0xAB -> o LAX Immediate   ; 0xA7 -> o LAX ZeroPage    ; 0xB7 -> o LAX ZeroPageY
    ; 0xAF -> o LAX Absolute    ; 0xBF -> o LAX AbsoluteY   ; 0xA3 -> o LAX IdxInd
    ; 0xB3 -> o LAX IndIdx      ; 0x87 -> o SAX ZeroPage    ; 0x97 -> o SAX ZeroPageY
    ; 0x8F -> o SAX Absolute    ; 0x83 -> o SAX IdxInd      ; 0xEB -> o SBC Immediate
    ; 0xC7 -> o DCP ZeroPage    ; 0xD7 -> o DCP ZeroPageX   ; 0xCF -> o DCP Absolute
    ; 0xDF -> o DCP AbsoluteX   ; 0xDB -> o DCP AbsoluteY   ; 0xC3 -> o DCP IdxInd
    ; 0xD3 -> o DCP IndIdx      ; 0xE7 -> o ISC ZeroPage    ; 0xF7 -> o ISC ZeroPageX
    ; 0xEF -> o ISC Absolute    ; 0xFF -> o ISC AbsoluteX   ; 0xFB -> o ISC AbsoluteY
    ; 0xE3 -> o ISC IdxInd      ; 0xF3 -> o ISC IndIdx      ; 0x27 -> o RLA ZeroPage
    ; 0x37 -> o RLA ZeroPageX   ; 0x2F -> o RLA Absolute    ; 0x3F -> o RLA AbsoluteX
    ; 0x3B -> o RLA AbsoluteY   ; 0x23 -> o RLA IdxInd      ; 0x33 -> o RLA IndIdx
    ; 0x67 -> o RRA ZeroPage    ; 0x77 -> o RRA ZeroPageX   ; 0x6F -> o RRA Absolute
    ; 0x7F -> o RRA AbsoluteX   ; 0x7B -> o RRA AbsoluteY   ; 0x63 -> o RRA IdxInd
    ; 0x73 -> o RRA IndIdx      ; 0x07 -> o SLO ZeroPage    ; 0x17 -> o SLO ZeroPageX
    ; 0x0F -> o SLO Absolute    ; 0x1F -> o SLO AbsoluteX   ; 0x1B -> o SLO AbsoluteY
    ; 0x03 -> o SLO IdxInd      ; 0x13 -> o SLO IndIdx      ; 0x47 -> o SRE ZeroPage
    ; 0x57 -> o SRE ZeroPageX   ; 0x4F -> o SRE Absolute    ; 0x5F -> o SRE AbsoluteX
    ; 0x5B -> o SRE AbsoluteY   ; 0x43 -> o SRE IdxInd      ; 0x53 -> o SRE IndIdx
    ; 0x0B -> o ANC Immediate   ; 0x2B -> o ANC Immediate   ; 0x4B -> o ALR Immediate
    ; 0x6B -> o ARR Immediate   ; 0x8B -> o XAA Immediate   ; 0x93 -> o AHX IndIdx
    ; 0x9F -> o AHX AbsoluteY   ; 0x9B -> o TAS AbsoluteY   ; 0x9E -> o SHX AbsoluteY
    ; 0x9C -> o SHY AbsoluteX   ; 0xBB -> o LAS AbsoluteY   ; 0xCB -> o AXS Immediate
    _ -> undefined -- Somehow GHC thinks 256 unique matches for a byte is non-exhaustive

data Instruction = Instruction OpCode [Word8]

showAMAndOP :: AddressMode -> [Word8] -> String
showAMAndOP am op = case am of
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

-- Some illegal opcodes are identical in behavior and addressing mode, we need
-- to look at the actual binary encoding if we want to distinguish them. Print
-- the hexadecimal encoding of the instruction right after the mnemonic so we
-- lose no information in the disassembly. For mnemonics where legal and illegal
-- variants exist, only flag the illegal ones as ambiguous so we don't clutter up
-- the standard opcode disassembly
isAmbiguousMn :: Word8 -> Mnemonic -> Bool
isAmbiguousMn w8 mn = case mn of
    ANC -> True
    KIL -> True
    NOP -> (w8 /= 0xEA) -- Only a single official variant
    SBC -> (w8 == 0xEB) -- All except one official
    _   -> False
showInstructionDisambiguate :: Instruction -> String
showInstructionDisambiguate (Instruction (viewOpCode -> OpCode w mn am) op) =
    show mn ++ (if isAmbiguousMn w mn then printf "($%02X)" w else "") ++ showAMAndOP am op

instance Show Instruction where
    show (Instruction (viewOpCode -> OpCode _ mn am) op) =
        show mn ++ showAMAndOP am op

{-# INLINE instructionLen #-}
instructionLen :: Instruction -> Int
instructionLen (Instruction (viewOpCode -> OpCode _ _ a) _) = 1 + operandLen a

decodeInstruction :: VU.Vector Word8 -> Int -> Maybe Instruction
decodeInstruction mem pc = do
    opMem <- mem VU.!? pc
    let opc@(viewOpCode -> OpCode _ _ am) = decodeOpCode opMem
    case operandLen am of
        -- TODO: We should probably just return a NOP in case the operands are missing
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
    opc@(viewOpCode -> OpCode _ _ am) <- decodeOpCode <$> (load8 $ Addr pc)
    Instruction opc <$> case operandLen am of
            1 -> mapM (load8 . Addr) [ pc + 1         ]
            2 -> mapM (load8 . Addr) [ pc + 1, pc + 2 ]
            _ -> return              [                ]

disassemble :: B.ByteString -> [B.ByteString]
disassemble bin = do
    let vec = VU.fromList $ B.unpack bin
        disassemble' pc =
            case decodeInstruction vec pc of
                Just instr -> let newPC   = pc + instructionLen instr
                                  showI   = B8.pack . showInstructionDisambiguate $ instr
                                  validPC = newPC < VU.length vec
                                  -- Build result using : instead of ++, no stack overflow etc.
                               in if validPC then showI : disassemble' newPC else [showI]
                Nothing -> []
     in disassemble' 0

