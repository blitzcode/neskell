
--{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, RankNTypes #-}

module Main where

import Instruction

import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
    bin <- B.readFile "./instr_test.bin"
    let vec = VU.fromList (B.unpack bin)
        disassemble pc = do
            let instr = decodeInstruction vec pc
                newPC = pc + instructionLen instr
            putStrLn . show $ instr
            when (newPC < VU.length vec) $ disassemble newPC
     in disassemble 0
    return ()

