
module Main where

import Data.Word (Word8, Word16)

data AddressMode =
      Accumulator
    | Immediate Word8
    | ZeroPage Word8
    | ZeroPageX Word8
    | ZeroPageY Word8
    | Relative Word8
    | Absolute Word16
    | AbsoluteX Word16
    | AbsoluteY Word16
    | Indirect Word16
    | IndexedIndirect Word8
    | IndirectIndexed Word8

main :: IO ()
main = return ()

