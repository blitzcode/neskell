
module CPU () where

import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8, Word16, Word64)

data CPUState = CPUState
    { cpuRAM   :: VU.Vector Word8
    , cpuPC    :: Word16
    , cpuA     :: Word8
    , cpuX     :: Word8
    , cpuY     :: Word8
    , cpuSR    :: Word8
    , cpuSP    :: Word8
    , cpuCycle :: Word64
    }

