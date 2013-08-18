
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
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

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
    let ring  = rbRing $ rb
        lenr  = fromIntegral $ VUM.length ring
        sUTF8 = encode s
    mapM_ (\(i, c) -> VUM.write ring (fromIntegral $ (ptr + i) `mod` lenr) c) $ zip [0..] sUTF8
    modifySTRef' (rbPtr rb) $ (+) (fromIntegral $ length sUTF8)

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

-- From Codec.Binary.UTF8.String
encode :: String -> [Word8]
encode = concatMap (map fromIntegral . go . ord)
    where go oc | oc <= 0x7F   = [oc]
                | oc <= 0x7FF  = [ 0xC0 + (oc `shiftR` 6)
                                 , 0x80 + oc .&. 0x3F
                                 ]
                | oc <= 0xFFFF = [ 0xE0 + (oc `shiftR` 12)
                                 , 0x80 + ((oc `shiftR` 6) .&. 0x3F)
                                 , 0x80 + oc .&. 0x3f
                                 ]
                | otherwise    = [ 0xF0 + (oc `shiftR` 18)
                                 , 0x80 + ((oc `shiftR` 12) .&. 0x3F)
                                 , 0x80 + ((oc `shiftR` 6) .&. 0x3F)
                                 , 0x80 + oc .&. 0x3F
                                 ]
   
