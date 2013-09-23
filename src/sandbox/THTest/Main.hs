
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main (main) where

import MainTH

$(makeExecute)

main :: IO ()
main = do
    mapM_ (\x -> putStr $ execute x Implied ++ " ") ([minBound..maxBound] :: [Mnemonic])
    return ()
