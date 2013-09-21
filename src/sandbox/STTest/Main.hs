
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Main where

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef, modifySTRef')
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word8)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)

data Env s = Env
    { envData :: VUM.MVector s Word8
    , envRef  :: STRef       s Int
    }

type ReaderEnvST s = ReaderT (Env s) (ST s)

class (Functor m, Monad m) => MonadDataAccess m where
    accessData :: Int -> m ()
    readData   :: m Int

instance MonadDataAccess (ReaderEnvST s) where
    {-# NOINLINE accessData #-}
    accessData val = do
        envdata <- asks envData
        lift $ mapM_ (\i -> VUM.write envdata i (fromIntegral val)) [0..VUM.length envdata - 1]
        envref <- asks envRef
        lift $ modifySTRef' envref (+ 1)
    {-# NOINLINE readData #-}
    readData = do
        envref  <- asks envRef
        envdata <- asks envData
        lift $ VUM.write envdata 84 99
        lift $ readSTRef envref

{-# NOINLINE runReaderST #-}
runReaderST :: (forall s. ReaderEnvST s a) -> a
runReaderST f = 
    runST $ do
        initData <- VUM.replicate (50 * 1024 * 1024) 0
        initRef  <- newSTRef 0
        let env = Env
                  { envData = initData
                  , envRef  = initRef
                  }
        runReaderT f env

{-# NOINLINE dataFunc #-}
dataFunc :: MonadDataAccess m => Int -> m ()
dataFunc val = do
    accessData val

{-# NOINLINE runStuff #-}
runStuff:: Int -> (Int, Int)
runStuff val =
    runReaderST $ do
        dataFunc val
        val2 <- readData
        dataFunc val
        return (sum [1..300000000 + val + val2], val2)

{-# NOINLINE checkResult #-}
checkResult :: (Int, Int) -> IO ()
checkResult (val1, val2) = do
    print val1
    print val2

main :: IO ()
main = do
    let res1 = runStuff 11
    checkResult res1
    let res2 = runStuff 12
    checkResult res2
    let res3 = runStuff 13
    checkResult res3
    let res4 = runStuff 14
    checkResult res4

