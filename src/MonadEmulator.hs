
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module MonadEmulator (MonadEmulator(..)
                     , LoadStore(..)
                     , runSTEmulator
                     , getTrace
                     , showCPUState
                     , Processor (..)
                     ) where

-- Exports the principal monad in which emulator code modifying the state of
-- the 6502 runs in. MonadEmulator provides a simple interface abstracting the
-- internal representation and monad transformer stack

import Util
import RingBufferLog

import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word8, Word16, Word64)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Control.Monad.Reader (ReaderT, runReaderT, local, ask, asks)
import Text.Printf
import qualified Data.ByteString.Lazy as B
import Control.Applicative (Applicative)
import Control.Monad (when)
import Control.Monad.Trans (lift)

data LoadStore = A | X | Y | SR | SP | PC | PCL | PCH | Addr Word16

instance Show LoadStore where
    show (Addr w) = printf "%04X" w
    show A   = "A"  ; show X   = "X"  ; show Y   = "Y"  ; show SR  = "SR" ; show SP  = "SP"
    show PC  = "PC" ; show PCL = "PCL"; show PCH = "PCH"

data Processor = NMOS_6502 | NES_2A03 deriving Eq

data CPUState s = CPUState
    { cpuState       :: VUM.MVector s Word8
    , cpuCycles      :: STRef       s Word64
    , cpuTraceRB     :: RingBuffer  s
    , cpuTraceEnable :: Bool
    , cpuModel       :: Processor
    }

showCPUState :: MonadEmulator m => Bool -> m String
showCPUState fieldNames = do
    a  <- load8 A
    x  <- load8 X
    y  <- load8 Y
    sr <- load8 SR
    sp <- load8 SP
    pc <- load16 PC
    c  <- getCycles
    return $ printf
        (if   fieldNames
         then "C:%07i PC:$%04X A:$%02X X:$%02X Y:$%02X SR:$%02X:%s SP:$%02X"
         else "%07i %04X %02X %02X %02X %02X:%s %02X")
         c pc a x y sr (makeSRString sr) sp

-- The 'standard' way of doing this would probably be using a newtype wrapper,
-- but with FlexibleInstances on this works and just seems simpler
type RSTEmu s = ReaderT (CPUState s) (ST s)

class (Monad m, Applicative m) => MonadEmulator m where
    load8      :: LoadStore -> m Word8
    load16     :: LoadStore -> m Word16
    store8     :: LoadStore -> Word8  -> m ()
    store16    :: LoadStore -> Word16 -> m ()
    trace      :: String -> m ()
    traceM     :: m String -> m ()
    runNoTrace :: m a -> m a
    advCycles  :: Word64 -> m ()
    getCycles  :: m Word64
    getModel   :: m Processor

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
        cpu <- ask
        when (cpuTraceEnable cpu) . lift $ writeRingBuffer (cpuTraceRB cpu) s
    -- The monadic version can be useful if we do a lot of MonadEmulator calls
    -- (load8 etc.) to produce the trace and would like to avoid those in case
    -- tracing is disabled
    traceM s = do
        cpu <- ask
        when (cpuTraceEnable cpu) $
            lift . writeRingBuffer (cpuTraceRB cpu) =<< s
    -- Disable tracing and run the argument function
    runNoTrace f = local (\cpu -> cpu { cpuTraceEnable = False }) f 
    advCycles n = do
        cycles <- asks cpuCycles
        lift $ modifySTRef' cycles (+ n)
    getCycles = do
        cycles <- asks cpuCycles
        lift $ readSTRef cycles
    getModel = asks cpuModel

-- We don't want to export this through MonadEmulator, only needs to be called
-- from code directly inside runSTEmulator's argument function. Preferably once,
-- at the end (note that 'unsafe' part...)
getTrace :: RSTEmu s B.ByteString
getTrace = do
    rb <- asks cpuTraceRB
    list <- lift $ unsafeFreezeRingBuffer rb
    return $ B.pack list

-- Need RankNTypes for the ST type
runSTEmulator :: Bool -> Int -> Processor -> (forall s. RSTEmu s a) -> a
runSTEmulator traceEnable traceMB processor f = 
    runST $ do
        initState   <- VUM.replicate (65536 + 7) (0 :: Word8)
        initCycles  <- newSTRef 0
        initTraceRB <- makeRingBuffer traceMB
        let cpu = CPUState
                  { cpuState        = initState
                  , cpuCycles       = initCycles
                  , cpuTraceEnable  = traceEnable
                  , cpuTraceRB      = initTraceRB
                  , cpuModel        = processor
                  }
        runReaderT f cpu

