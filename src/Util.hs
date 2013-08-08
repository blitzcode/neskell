
module Util ( makeW16
            , splitW16
            , loadPC
            , storePC
            , updatePC
            ) where

import MonadEmulator (MonadEmulator(..), LoadStore(..))

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8, Word16)
import Control.Monad (liftM, liftM2)

makeW16 :: Word8 -> Word8 -> Word16
makeW16 l h = (fromIntegral l :: Word16) .|. (fromIntegral h :: Word16) `shiftL` 8

splitW16 :: Word16 -> (Word8, Word8)
splitW16 w = (fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8))

loadPC :: MonadEmulator m => m Word16
loadPC = liftM2 (makeW16) (load PCL) (load PCH)

storePC :: MonadEmulator m => Word16 -> m ()
storePC pc = case splitW16 pc of (l, h) -> store PCL l >> store PCH h

updatePC :: MonadEmulator m => (Word16 -> Word16) -> m ()
updatePC f = loadPC >>= return . f >>= storePC

