
module Util ( makeW16
            , splitW16
            , loadPC
            , storePC
            , updatePC
            , cpuState
            ) where

import MonadEmulator (MonadEmulator(..), LoadStore(..))

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8, Word16)
import Control.Monad (liftM, liftM2)
import Text.Printf

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

cpuState :: MonadEmulator m => m String
cpuState = do
    a  <- load A
    x  <- load X
    y  <- load Y
    sr <- load SR
    sp <- load SP
    pc <- loadPC
    return $ printf "A: 0x%02X X: 0x%02X Y: 0x%02X SR: 0x%02X SP: 0x%02X PC: 0x%04X"
        a x y sr sp pc

