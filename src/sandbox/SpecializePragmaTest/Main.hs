
module Main (main) where

import STImpl

import Control.Monad

{-# SPECIALIZE INLINE useAbstractMonad :: ReaderST s Int #-}
useAbstractMonad :: MonadAbstractIOST m => m Int
useAbstractMonad = foldM (\a b -> a `seq` return . (a +) =<< (addstuff b)) 0 [1..50000000]

-- useConcreteMonad :: ReaderST s Int
-- useConcreteMonad = foldM (\a b -> a `seq` return . (a +) =<< (addstuff b)) 0 [1..50000000]

main :: IO ()
main = do
    let st = runAbstractST useAbstractMonad
    putStrLn . show $ st

