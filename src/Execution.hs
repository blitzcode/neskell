
module Execution ( execute
                 ) where

-- The actual emulation of all 6502 instructions running inside of MonadEmulator

import MonadEmulator
import Instruction
import Util

import Data.Word (Word8, Word16)

-- data AddressMode = Implied | Accumulator | Immediate | ZeroPage | ZeroPageX | ZeroPageY | Relative | Absolute | AbsoluteX | AbsoluteY | Indirect | IdxInd | IndIdx
-- data OpCode = OpCode Mnemonic AddressMode
-- data Instruction = Instruction OpCode [Word8]
-- instructionLen :: Instruction -> Int

execute :: MonadEmulator m => Instruction -> m ()
execute inst = do
    updatePC ((fromIntegral $ instructionLen inst) +)
    case inst of
        Instruction (OpCode LDA am) oper -> return ()
        _ -> return ()

