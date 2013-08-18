
module RingBufferLog ( RingBuffer
                     , makeRingBuffer
                     , writeRingBuffer
                     , unsafeFreezeRingBuffer
                     ) where

-- Ring buffer in the ST monad, made for logging lines of text

import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.ByteString.Internal (c2w)
import Data.Word (Word8, Word32)

data RingBuffer s = RingBuffer
    { rbRing :: VUM.MVector s Word8
    , rbPtr  :: STRef       s Word32 -- Should be Word64, but there's quite a slowdown
    }

makeRingBuffer :: Int -> ST s (RingBuffer s)
makeRingBuffer sizeMB = do
    initRing <- VUM.new (sizeMB * 1024 * 1024)
    initPtr  <- newSTRef 0
    return RingBuffer { rbRing = initRing
                      , rbPtr  = initPtr
                      }

writeRingBuffer :: RingBuffer s -> String -> ST s ()
writeRingBuffer rb s = do
        ptr <- readSTRef $ rbPtr rb
        let ring = rbRing $ rb
            lenr = fromIntegral $ VUM.length ring
        mapM_ (\(i, c) -> VUM.write ring (fromIntegral $ (ptr + i) `mod` lenr) (c2w c)) $ zip [0..] s
        modifySTRef' (rbPtr rb) $ (+) (fromIntegral $ length s)

-- It is no longer safe to write to the ring buffer after calling this
unsafeFreezeRingBuffer :: RingBuffer s -> ST s [Word8]
unsafeFreezeRingBuffer rb = do
    ptr <- readSTRef $ rbPtr rb
    vec <- VU.unsafeFreeze $ rbRing rb
    return $ ringBufferToList ptr vec

-- Convert a ring buffer into a lazy list of its contents
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

