
module Execution ( execute
                 ) where

-- The actual emulation of all 6502 instructions running inside of MonadEmulator

import MonadEmulator
import Instruction
import Util

import Data.Word (Word8, Word16)
import qualified Data.ByteString.Char8 as B8
import Text.Printf

-- data AddressMode = Implied | Accumulator | Immediate | ZeroPage | ZeroPageX | ZeroPageY | Relative | Absolute | AbsoluteX | AbsoluteY | Indirect | IdxInd | IndIdx
-- data OpCode = OpCode Mnemonic AddressMode
-- data Instruction = Instruction OpCode [Word8]
-- instructionLen :: Instruction -> Int
-- data LoadStore = A | X | Y | SR | SP | PCL | PCH | Addr Word16

loadOperand :: MonadEmulator m => Instruction -> m (Either Word8 Word16)
loadOperand (Instruction (OpCode _ am) oper) = return $ Left 5

execute :: MonadEmulator m => Instruction -> m ()
execute inst = do
    trace . B8.pack $ printf "\n%s (%ib): " (show inst) (instructionLen inst)
    updatePC ((fromIntegral $ instructionLen inst) +)
    case inst of
        Instruction (OpCode LDA am) oper -> return ()
        _ -> return ()
    cpustate <- cpuState
    trace . B8.pack . printf "\n%s\n" $ cpustate

