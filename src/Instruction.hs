
module Instruction ( AddressMode(..)
                   , operandLen
                   , Mnemonic(..)
                   , OpCode(..)
                   , decodeOpCode
                   , Instruction(..)
                   , instructionLen
                   , decodeInstruction
                   ) where

-- This module contains types and function for representing and decoding
-- all official instructions of the 6502

import Data.Word (Word8)
import Text.Printf
import Data.Bits (shiftL, (.|.))
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
      ADC | AND | ASL | BCC | BCS | BEQ
    | BIT | BMI | BNE | BPL | BRK | BVC
    | BVS | CLC | CLD | CLI | CLV | CMP
    | CPX | CPY | DEC | DEX | DEY | EOR
    | INC | INX | INY | JMP | JSR | LDA
    | LDX | LDY | LSR | NOP | ORA | PHA
    | PHP | PLA | PLP | ROL | ROR | RTI
    | RTS | SBC | SEC | SED | SEI | STA
    | STX | STY | TAX | TAY | TSX | TXA
    | TXS | TYA | DCB Word8
      deriving (Show)

data OpCode = OpCode Mnemonic AddressMode

decodeOpCode :: Word8 -> OpCode
decodeOpCode opc =
    case opc of
        ; 0x69 -> OpCode ADC Immediate   ; 0x65 -> OpCode ADC ZeroPage    ; 0x75 -> OpCode ADC ZeroPageX   
        ; 0x6D -> OpCode ADC Absolute    ; 0x7D -> OpCode ADC AbsoluteX   ; 0x79 -> OpCode ADC AbsoluteY   
        ; 0x61 -> OpCode ADC IdxInd      ; 0x71 -> OpCode ADC IndIdx      ; 0x29 -> OpCode AND Immediate   
        ; 0x25 -> OpCode AND ZeroPage    ; 0x35 -> OpCode AND ZeroPageX   ; 0x2D -> OpCode AND Absolute    
        ; 0x3D -> OpCode AND AbsoluteX   ; 0x39 -> OpCode AND AbsoluteY   ; 0x21 -> OpCode AND IdxInd      
        ; 0x31 -> OpCode AND IndIdx      ; 0x0A -> OpCode ASL Accumulator ; 0x06 -> OpCode ASL ZeroPage    
        ; 0x16 -> OpCode ASL ZeroPageX   ; 0x0E -> OpCode ASL Absolute    ; 0x1E -> OpCode ASL AbsoluteX   
        ; 0x90 -> OpCode BCC Relative    ; 0xB0 -> OpCode BCS Relative    ; 0xF0 -> OpCode BEQ Relative    
        ; 0x24 -> OpCode BIT ZeroPage    ; 0x2C -> OpCode BIT Absolute    ; 0x30 -> OpCode BMI Relative    
        ; 0xD0 -> OpCode BNE Relative    ; 0x10 -> OpCode BPL Relative    ; 0x00 -> OpCode BRK Implied     
        ; 0x50 -> OpCode BVC Relative    ; 0x70 -> OpCode BVS Relative    ; 0x18 -> OpCode CLC Implied     
        ; 0xD8 -> OpCode CLD Implied     ; 0x58 -> OpCode CLI Implied     ; 0xB8 -> OpCode CLV Implied     
        ; 0xC9 -> OpCode CMP Immediate   ; 0xC5 -> OpCode CMP ZeroPage    ; 0xD5 -> OpCode CMP ZeroPageX   
        ; 0xCD -> OpCode CMP Absolute    ; 0xDD -> OpCode CMP AbsoluteX   ; 0xD9 -> OpCode CMP AbsoluteY   
        ; 0xC1 -> OpCode CMP IdxInd      ; 0xD1 -> OpCode CMP IndIdx      ; 0xE0 -> OpCode CPX Immediate   
        ; 0xE4 -> OpCode CPX ZeroPage    ; 0xEC -> OpCode CPX Absolute    ; 0xC0 -> OpCode CPY Immediate   
        ; 0xC4 -> OpCode CPY ZeroPage    ; 0xCC -> OpCode CPY Absolute    ; 0xC6 -> OpCode DEC ZeroPage    
        ; 0xD6 -> OpCode DEC ZeroPageX   ; 0xCE -> OpCode DEC Absolute    ; 0xDE -> OpCode DEC AbsoluteX   
        ; 0xCA -> OpCode DEX Implied     ; 0x88 -> OpCode DEY Implied     ; 0x49 -> OpCode EOR Immediate   
        ; 0x45 -> OpCode EOR ZeroPage    ; 0x55 -> OpCode EOR ZeroPageX   ; 0x4D -> OpCode EOR Absolute    
        ; 0x5D -> OpCode EOR AbsoluteX   ; 0x59 -> OpCode EOR AbsoluteY   ; 0x41 -> OpCode EOR IdxInd      
        ; 0x51 -> OpCode EOR IndIdx      ; 0xE6 -> OpCode INC ZeroPage    ; 0xF6 -> OpCode INC ZeroPageX   
        ; 0xEE -> OpCode INC Absolute    ; 0xFE -> OpCode INC AbsoluteX   ; 0xE8 -> OpCode INX Implied     
        ; 0xC8 -> OpCode INY Implied     ; 0x4C -> OpCode JMP Absolute    ; 0x6C -> OpCode JMP Indirect    
        ; 0x20 -> OpCode JSR Absolute    ; 0xA9 -> OpCode LDA Immediate   ; 0xA5 -> OpCode LDA ZeroPage    
        ; 0xB5 -> OpCode LDA ZeroPageX   ; 0xAD -> OpCode LDA Absolute    ; 0xBD -> OpCode LDA AbsoluteX   
        ; 0xB9 -> OpCode LDA AbsoluteY   ; 0xA1 -> OpCode LDA IdxInd      ; 0xB1 -> OpCode LDA IndIdx      
        ; 0xA2 -> OpCode LDX Immediate   ; 0xA6 -> OpCode LDX ZeroPage    ; 0xB6 -> OpCode LDX ZeroPageY   
        ; 0xAE -> OpCode LDX Absolute    ; 0xBE -> OpCode LDX AbsoluteY   ; 0xA0 -> OpCode LDY Immediate   
        ; 0xA4 -> OpCode LDY ZeroPage    ; 0xB4 -> OpCode LDY ZeroPageX   ; 0xAC -> OpCode LDY Absolute    
        ; 0xBC -> OpCode LDY AbsoluteX   ; 0x4A -> OpCode LSR Accumulator ; 0x46 -> OpCode LSR ZeroPage    
        ; 0x56 -> OpCode LSR ZeroPageX   ; 0x4E -> OpCode LSR Absolute    ; 0x5E -> OpCode LSR AbsoluteX   
        ; 0xEA -> OpCode NOP Implied     ; 0x09 -> OpCode ORA Immediate   ; 0x05 -> OpCode ORA ZeroPage    
        ; 0x15 -> OpCode ORA ZeroPageX   ; 0x0D -> OpCode ORA Absolute    ; 0x1D -> OpCode ORA AbsoluteX   
        ; 0x19 -> OpCode ORA AbsoluteY   ; 0x01 -> OpCode ORA IdxInd      ; 0x11 -> OpCode ORA IndIdx      
        ; 0x48 -> OpCode PHA Implied     ; 0x08 -> OpCode PHP Implied     ; 0x68 -> OpCode PLA Implied     
        ; 0x28 -> OpCode PLP Implied     ; 0x2A -> OpCode ROL Accumulator ; 0x26 -> OpCode ROL ZeroPage    
        ; 0x36 -> OpCode ROL ZeroPageX   ; 0x2E -> OpCode ROL Absolute    ; 0x3E -> OpCode ROL AbsoluteX   
        ; 0x6A -> OpCode ROR Accumulator ; 0x66 -> OpCode ROR ZeroPage    ; 0x76 -> OpCode ROR ZeroPageX   
        ; 0x6E -> OpCode ROR Absolute    ; 0x7E -> OpCode ROR AbsoluteX   ; 0x40 -> OpCode RTI Implied     
        ; 0x60 -> OpCode RTS Implied     ; 0xE9 -> OpCode SBC Immediate   ; 0xE5 -> OpCode SBC ZeroPage    
        ; 0xF5 -> OpCode SBC ZeroPageX   ; 0xED -> OpCode SBC Absolute    ; 0xFD -> OpCode SBC AbsoluteX   
        ; 0xF9 -> OpCode SBC AbsoluteY   ; 0xE1 -> OpCode SBC IdxInd      ; 0xF1 -> OpCode SBC IndIdx      
        ; 0x38 -> OpCode SEC Implied     ; 0xF8 -> OpCode SED Implied     ; 0x78 -> OpCode SEI Implied     
        ; 0x85 -> OpCode STA ZeroPage    ; 0x95 -> OpCode STA ZeroPageX   ; 0x8D -> OpCode STA Absolute    
        ; 0x9D -> OpCode STA AbsoluteX   ; 0x99 -> OpCode STA AbsoluteY   ; 0x81 -> OpCode STA IdxInd      
        ; 0x91 -> OpCode STA IndIdx      ; 0x86 -> OpCode STX ZeroPage    ; 0x96 -> OpCode STX ZeroPageY   
        ; 0x8E -> OpCode STX Absolute    ; 0x84 -> OpCode STY ZeroPage    ; 0x94 -> OpCode STY ZeroPageX   
        ; 0x8C -> OpCode STY Absolute    ; 0xAA -> OpCode TAX Implied     ; 0xA8 -> OpCode TAY Implied     
        ; 0xBA -> OpCode TSX Implied     ; 0x8A -> OpCode TXA Implied     ; 0x9A -> OpCode TXS Implied     
        ; 0x98 -> OpCode TYA Implied     ; _    -> OpCode (DCB opc) Implied

data Instruction = Instruction OpCode [Word8]

makeW16 :: Word8 -> Word8 -> Int
makeW16 l h = (fromIntegral l :: Int) .|. (fromIntegral h :: Int) `shiftL` 8

instance Show Instruction where
    -- Special case: DCB is not an instruction, but a mnemonic for embedding raw
    -- bytes into the assembly. We decode all illegal opcodes to it
    show (Instruction (OpCode (DCB b) Implied) []) = printf "DCB #$%02X" b
    show (Instruction (OpCode mn am) op) =
        show mn ++ " " ++
        case am of
            Implied     -> case op of []           ->        ""                            ; _ -> "OpLnErr"
            Accumulator -> case op of []           ->        "A"                           ; _ -> "OpLnErr"
            Immediate   -> case op of [opl]        -> printf "#$%02X"              opl     ; _ -> "OpLnErr"
            ZeroPage    -> case op of [opl]        -> printf "$%02X"               opl     ; _ -> "OpLnErr"
            ZeroPageX   -> case op of [opl]        -> printf "$%02X,X"             opl     ; _ -> "OpLnErr"
            ZeroPageY   -> case op of [opl]        -> printf "$%02X,Y"             opl     ; _ -> "OpLnErr"
            Relative    -> case op of [opl]        -> printf "$%02X"               opl     ; _ -> "OpLnErr"
            Absolute    -> case op of (opl:oph:[]) -> printf "$%04X"     $ makeW16 opl oph ; _ -> "OpLnErr"
            AbsoluteX   -> case op of (opl:oph:[]) -> printf "$%04X,X"   $ makeW16 opl oph ; _ -> "OpLnErr"
            AbsoluteY   -> case op of (opl:oph:[]) -> printf "$%04X,Y"   $ makeW16 opl oph ; _ -> "OpLnErr"
            Indirect    -> case op of (opl:oph:[]) -> printf "($%04X)"   $ makeW16 opl oph ; _ -> "OpLnErr"
            IdxInd      -> case op of [opl]        -> printf "($%02X,X)"           opl     ; _ -> "OpLnErr"
            IndIdx      -> case op of [opl]        -> printf "($%02X),Y"           opl     ; _ -> "OpLnErr"

instructionLen :: Instruction -> Int
instructionLen (Instruction (OpCode _ a) _) = 1 + operandLen a

decodeInstruction :: VU.Vector Word8 -> Int -> Instruction
decodeInstruction mem pc =
    let opc@(OpCode _ am) = decodeOpCode $ mem VU.! pc
     in case operandLen am of
            1 -> Instruction opc [ mem VU.! (pc + 1)                    ]
            2 -> Instruction opc [ mem VU.! (pc + 1), mem VU.! (pc + 2) ]
            _ -> Instruction opc [                                      ]

