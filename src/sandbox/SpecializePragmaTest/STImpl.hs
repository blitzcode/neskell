
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module STImpl (runAbstractST, MonadAbstractIOST(..), ReaderST) where

import Control.Monad.Reader
import Control.Monad.ST
import Control.Applicative

class (Applicative m, {- Functor m, -} Monad m) => MonadAbstractIOST m where
    addstuff :: Int -> m Int

type ReaderST s = ReaderT (Int) (ST s)

instance MonadAbstractIOST (ReaderST s) where
    addstuff a = return . (a +) =<< ask

runAbstractST :: (forall s. ReaderST s a) -> a
runAbstractST f = runST $ runReaderT f 99

