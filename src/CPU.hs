
--{-# LANGUAGE RankNTypes #-}

module CPU () where

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString as B
import Data.Word (Word8, Word16, Word64)

--import Control.Applicative ((<$>))
import Control.Monad.ST (ST, runST)
import Data.STRef

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)

data CPUState s = CPUState
    { cpuRAM   :: VUM.MVector s Word8
    , cpuPC    :: STRef s Word16
    , cpuA     :: STRef s Word8
    , cpuX     :: STRef s Word8
    , cpuY     :: STRef s Word8
    , cpuSR    :: STRef s Word8
    , cpuSP    :: STRef s Word8
    , cpuCycle :: STRef s Word64
    }

type RSTEmu s a = ReaderT (CPUState s) (ST s) a

runCPU :: RSTEmu s ()
runCPU = do
    return ()

loadBinary :: B.ByteString -> Word16 -> RSTEmu s ()
loadBinary bin offs = do
    ram <- asks cpuRAM
    mapM_ (\i -> lift $
        VUM.write ram (fromIntegral offs + i) $
        B.index   bin (fromIntegral offs + i)) [0..B.length bin]

runEmulator :: B.ByteString -> Word16 -> Word16 -> Int
runEmulator bin offs pc =
    runST $ do
        initRAM   <- VUM.replicate (2 ^ (16 :: Int)) (0 :: Word8)
        initPC    <- newSTRef pc
        initA     <- newSTRef 0
        initX     <- newSTRef 0
        initY     <- newSTRef 0
        initSR    <- newSTRef 0
        initSP    <- newSTRef 0xFF
        initCycle <- newSTRef 0
        let cpu = CPUState
                  { cpuRAM   = initRAM
                  , cpuPC    = initPC
                  , cpuA     = initA
                  , cpuX     = initX
                  , cpuY     = initY
                  , cpuSR    = initSR
                  , cpuSP    = initSP
                  , cpuCycle = initCycle
                  }
        runReaderT (do
            loadBinary bin offs
            runCPU
            :: RSTEmu s ()) cpu
        return 0

