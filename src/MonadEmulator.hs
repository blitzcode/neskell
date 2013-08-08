
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module MonadEmulator (MonadEmulator(..)
                     , LoadStore(..)
                     , runSTEmulator
                     , getTrace
                     ) where

-- Exports the principal monad in which emulator code modifying the state of
-- the 6502 runs in. MonadEmulator provides a simple interface abstracting the
-- internal representation and monad transformer stack

import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word8, Word16, Word64)
import Control.Monad.ST (ST, runST)
import Data.STRef
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad (when)
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Builder as BB

data LoadStore = A | X | Y | SR | SP | PCL | PCH | Addr Word16

instance Show LoadStore where
    show (Addr w) = printf "Addr 0x%04X" w
    show A   = "A"  ; show X   = "X"  ; show Y   = "Y"  ; show SR  = "SR" ; show SP  = "SP"
    show PCL = "PCL"; show PCH = "PCH"

data CPUState s = CPUState
    { cpuState       :: VUM.MVector s Word8
    , cpuCycle       :: STRef       s Word64
    , cpuTrace       :: STRef       s B.ByteString -- TODO: Use a ByteString builder
    , cpuTraceEnable :: Bool
    }

-- The 'standard' way of doing this would probably be using a newtype wrapper,
-- but with FlexibleInstances on this works and just seems simpler
type RSTEmu s = ReaderT (CPUState s) (ST s)

class (Functor m, Monad m) => MonadEmulator m where
    load  :: LoadStore -> m Word8
    store :: LoadStore -> Word8 -> m ()
    trace :: B.ByteString -> m ()

lsToStateIdx :: LoadStore -> Int
lsToStateIdx ls =
    case ls of
        -- We store the 7 bytes of registers at the end of the regular 2^16 address space
        A      -> rbase + 0
        X      -> rbase + 1
        Y      -> rbase + 2
        SR     -> rbase + 3
        SP     -> rbase + 4
        PCL    -> rbase + 5
        PCH    -> rbase + 6
        Addr a -> fromIntegral a
  where
    rbase = 65536

instance MonadEmulator (RSTEmu s) where
    load ls = do
        state <- asks cpuState
        lift $ VUM.read state . lsToStateIdx $ ls
    store ls val = do
        state <- asks cpuState
        trace . B8.pack $ printf "0x%02X -> %s" val (show ls)
        lift $ VUM.write state (lsToStateIdx ls) val
    trace b = do
         enable <- asks cpuTraceEnable
         when (enable) $ do
            cputrace <- asks cpuTrace 
            lift $ modifySTRef cputrace (\log -> log `B.append` b `B8.snoc` '\n')

-- We don't want to export this through MonadEmulator, only needs to be called
-- from code directly inside runSTEmulator's argument function
getTrace :: RSTEmu s B.ByteString
getTrace = do
    cputrace <- asks cpuTrace
    lift $ readSTRef cputrace

runSTEmulator :: Bool -> (forall s. RSTEmu s a) -> a -- Need RankNTypes for the ST type magic
runSTEmulator trace f = 
    runST $ do
        initState <- VUM.replicate (65536 + 7) (0 :: Word8)
        initCycle <- newSTRef 0
        initTrace <- newSTRef B.empty
        let cpu = CPUState
                  { cpuState       = initState
                  , cpuCycle       = initCycle
                  , cpuTrace       = initTrace
                  , cpuTraceEnable = trace
                  }
        runReaderT f cpu

