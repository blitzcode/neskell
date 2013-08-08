
module Execution ( execute
                 ) where

-- The actual emulation of all 6502 instructions running inside of MonadEmulator

import MonadEmulator
import Instruction

execute :: MonadEmulator m => Instruction -> m ()
execute inst = return ()

