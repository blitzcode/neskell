
module Emulator ( runEmulator
                , TerminationCond(..)
                ) where

-- Emulator main loop. Load a binary, set up the CPU and then run until a
-- termination criterea has been met

import MonadEmulator
import Execution
import qualified Instruction as I

import qualified Data.ByteString as B
import Data.Word (Word8, Word16, Word64)
import Control.Monad (unless)

data TerminationCond = TermNever | TermOnPC Word16 | TermOnOpC I.OpCode | TermOnCycleGT Word64

checkTC :: MonadEmulator m => TerminationCond -> m Bool
checkTC tc =
    case tc of
        TermNever       -> return False
        TermOnPC pc     -> return False
        TermOnOpC opc   -> return False
        TermOnCycleGT c -> return False

loadBinary :: MonadEmulator m => B.ByteString -> Word16 -> m ()
loadBinary bin offs = do
    mapM_ (\i ->
        store (Addr $ offs + fromIntegral i) $
        B.index bin (fromIntegral offs + i)) [0..B.length bin]

runEmulator :: B.ByteString -> Word16 -> Word16 -> TerminationCond -> Int
runEmulator bin offs pc tc =
    runSTEmulator $ do
        loadBinary bin offs
        let loop = do
                terminate <- checkTC tc
                unless terminate loop
         in loop
        return 0

