
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main (main) where

import MainTH
import Instruction
import MonadEmulator

$(makeExecute)

main :: IO ()
main = do
    -- mapM_ (\x -> execute x Implied) ([minBound..maxBound] :: [Mnemonic])
    return $ runSTEmulator False 1 NMOS_6502 $ do
        mapM_ (execute) [decodeOpCode w8 | w8 <- [0..255]]
    return ()

