
module Util ( L8R16
            , makeW16
            , splitW16
            , Flag(..)
            , getFlag
            , setFlag
            , clearFlag
            , modifyFlag
            , makeSRString
            , srFromString
            , b2W8) where

import Data.Word (Word8, Word16)
import Data.Bits

type L8R16 = Either Word8 Word16

makeW16 :: Word8 -> Word8 -> Word16
makeW16 l h = (fromIntegral l :: Word16) .|. (fromIntegral h :: Word16) `shiftL` 8

splitW16 :: Word16 -> (Word8, Word8)
splitW16 w = (fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8))

data Flag = FN | FV | F1 | FB | FD | FI | FZ | FC
            deriving (Enum)

getFlag :: Flag -> Word8 -> Bool
getFlag f w = testBit w (7 - fromEnum f)

setFlag :: Flag -> Word8 -> Word8
setFlag f w = setBit w (7 - fromEnum f)

clearFlag :: Flag -> Word8 -> Word8
clearFlag f w = clearBit w (7 - fromEnum f)

modifyFlag :: Flag -> Bool -> Word8 -> Word8
modifyFlag f b w = if b then setFlag f w else clearFlag f w

makeSRString :: Word8 -> String
makeSRString w =
    map (\(f, s) -> if getFlag f w then s else '·') $ zip
        [FN .. FC] ['N', 'V', '1', 'B', 'D', 'I', 'Z', 'C']

srFromString :: String -> Word8
srFromString s = foldr (\(c, f) sr -> modifyFlag f (c /= '-' && c /= '·') sr) 0 $ zip s [FN .. FC]

b2W8 :: Bool -> Word8
b2W8 b = if b then 1 else 0

