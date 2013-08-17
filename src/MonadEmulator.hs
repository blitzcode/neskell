
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module MonadEmulator (MonadEmulator(..)
                     , LoadStore(..)
                     , runSTEmulator
                     , getTrace
                     , showCPUState
                     ) where

-- Exports the principal monad in which emulator code modifying the state of
-- the 6502 runs in. MonadEmulator provides a simple interface abstracting the
-- internal representation and monad transformer stack

import Util

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8, Word16, Word32, Word64)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Control.Monad.Reader
import Text.Printf
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Internal (c2w)

import Data.Bits
import Data.Char (ord)

data LoadStore = A | X | Y | SR | SP | PC | PCL | PCH | Addr Word16

instance Show LoadStore where
    show (Addr w) = printf "Addr 0x%04X" w
    show A   = "A"  ; show X   = "X"  ; show Y   = "Y"  ; show SR  = "SR" ; show SP  = "SP"
    show PC  = "PC" ; show PCL = "PCL"; show PCH = "PCH"

data CPUState s = CPUState
    { cpuState        :: VUM.MVector s Word8
    , cpuCycles       :: STRef       s Word64
    , cpuTraceRing    :: VUM.MVector s Word8
    , cpuTraceRingPtr :: STRef       s Word32 -- Should be Word64, but there's quite a slowdown
    , cpuTraceEnable  :: Bool
    }

showCPUState :: MonadEmulator m => m String
showCPUState = do
    a  <- load8 A
    x  <- load8 X
    y  <- load8 Y
    sr <- load8 SR
    sp <- load8 SP
    pc <- load16 PC
    c  <- getCycles
    return $ printf "A:0x%02X X:0x%02X Y:0x%02X SR:0x%02X:%s SP:0x%02X PC:0x%04X Cycles:%09i"
        a x y sr (makeSRString sr) sp pc c

-- The 'standard' way of doing this would probably be using a newtype wrapper,
-- but with FlexibleInstances on this works and just seems simpler
type RSTEmu s = ReaderT (CPUState s) (ST s)

class (Functor m, Monad m) => MonadEmulator m where
    load8     :: LoadStore -> m Word8
    load16    :: LoadStore -> m Word16
    store8    :: LoadStore -> Word8  -> m ()
    store16   :: LoadStore -> Word16 -> m ()
    trace     :: String -> m ()
    advCycles :: Word64 -> m ()
    getCycles :: m Word64

lsToStateIdx :: LoadStore -> Int
lsToStateIdx ls =
    case ls of
        -- We store the 7 bytes of registers at the end of the regular 2^16 address space
        A      -> rbase + 0
        X      -> rbase + 1
        Y      -> rbase + 2
        SR     -> rbase + 3
        SP     -> rbase + 4
        PC     -> rbase + 5 -- Aliases with PCL/PCH, as expected
        PCL    -> rbase + 5
        PCH    -> rbase + 6
        Addr a -> fromIntegral a
  where
    rbase = 65536

instance MonadEmulator (RSTEmu s) where
    load8 ls = do
        state <- asks cpuState
        lift $ VUM.read state . lsToStateIdx $ ls
    load16 ls = do
        let e8 = error "16 bit load from 8 bit register"
        state <- asks cpuState
        case ls of
            Addr 65535 -> e8; A -> e8; X -> e8; Y -> e8; SR -> e8; SP -> e8; PCL -> e8; PCH -> e8
            _ -> lift $ do let i = lsToStateIdx ls
                           l <- VUM.read state i
                           h <- VUM.read state (i + 1)
                           return $ makeW16 l h
    store8 ls val = do
        state <- asks cpuState
        lift $ VUM.write state (lsToStateIdx ls) val
    store16 ls val = do
        let e8 = error "16 bit store to 8 bit register"
        state <- asks cpuState
        case ls of
            Addr 65535 -> e8; A -> e8; X -> e8; Y -> e8; SR -> e8; SP -> e8; PCL -> e8; PCH -> e8
            _ -> lift $ do let (l, h) = splitW16 val
                           let i = lsToStateIdx ls
                           VUM.write state i l
                           VUM.write state (i + 1) h
    trace s = do
        enable <- asks cpuTraceEnable
        when (enable) $ ringBufferWrite s
    advCycles n = do
        cycles <- asks cpuCycles
        lift $ modifySTRef' cycles (+ n)
    getCycles = do
        cycles <- asks cpuCycles
        lift $ readSTRef cycles

ringBufferWrite :: String -> RSTEmu s ()
ringBufferWrite s = do
    ring <- asks cpuTraceRing
    ptrR <- asks cpuTraceRingPtr
    lift $ do
        ptr <- readSTRef ptrR
        let lenr = fromIntegral $ VUM.length ring
        mapM_ (\(i, c) -> VUM.write ring (fromIntegral $ (ptr + i) `mod` lenr) (c2w c)) $ zip [0..] s
        modifySTRef' ptrR $ (+) (fromIntegral $ length s)

-- Convert a ring buffer vector and a write marker into a lazy list of its contents
ringBufferToList :: Word32 -> VU.Vector Word8 -> [Word8]
ringBufferToList ptr' vec =
    let len = fromIntegral $ VU.length vec
        loop ptr left foundcr =
            let e    = vec VU.! (fromIntegral $ ptr `mod` len)
                next = loop (ptr + 1) (left - 1)
                r | left == 0   = []
                  -- Don't start on an incomplete line, skip till the first CR
                  | not foundcr = next $ e == c2w '\n'
                  | otherwise   = e : next True
             in r
     in   if ptr' < len       -- Already wrapped around?
        then loop 0 ptr' True -- No, start at the beginning of the vector
        -- Yes, the beginning is right after the write marker
        else map c2w "(Trace Truncated)\n\n" ++ loop (ptr' + 1) (len - 1) False

-- We don't want to export this through MonadEmulator, only needs to be called
-- from code directly inside runSTEmulator's argument function. Preferably once,
-- at the end (note that 'unsafe' part...)
getTrace :: RSTEmu s B.ByteString
getTrace = do
    ring   <- asks cpuTraceRing
    ptrref <- asks cpuTraceRingPtr
    ptr    <- lift $ readSTRef ptrref
    frozen <- lift $ VU.unsafeFreeze ring
    return . B.pack . ringBufferToList ptr $ frozen

runSTEmulator :: Bool -> Int -> (forall s. RSTEmu s a) -> a -- Need RankNTypes for the ST type magic
runSTEmulator traceEnable traceMB f = 
    runST $ do
        initState        <- VUM.replicate (65536 + 7) (0 :: Word8)
        initCycles       <- newSTRef 0
        initTraceRing    <- VUM.new (traceMB * 1024 * 1024)
        initTraceRingPtr <- newSTRef 0
        let cpu = CPUState
                  { cpuState        = initState
                  , cpuCycles       = initCycles
                  , cpuTraceEnable  = traceEnable
                  , cpuTraceRing    = initTraceRing
                  , cpuTraceRingPtr = initTraceRingPtr
                  }
        runReaderT f cpu

