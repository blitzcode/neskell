
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (All(..))

maybeIOTest :: MaybeT IO ()
maybeIOTest = do
    foo <- return $ Nothing
    liftIO $ putStrLn "TestMaybeIO"
    bar <- return $ 31
    guard False
    liftIO $ putStrLn "TestMaybeIO_2nd"

writerIOTest :: WriterT All IO ()
writerIOTest = do
    liftIO $ putStrLn "TestWriterIO"
    tell $ All False
    return ()

writerMaybeIOTest :: WriterT All (MaybeT IO) ()
writerMaybeIOTest = do
    liftIO $ putStrLn "TestWriterMaybeIO"
    guard False
    tell $ All False
    liftIO $ putStrLn "TestWriterMaybeIO_2nd"
    return ()

maybeWriterIOTest :: MaybeT (WriterT All IO) ()
maybeWriterIOTest = do
    liftIO $ putStrLn "TestWriterMaybeIO"
    guard False
    tell $ All False
    liftIO $ putStrLn "TestWriterMaybeIO_2nd"
    return ()

testRun :: IO ()
testRun = do
    (a, w) <- runWriterT writerIOTest
    resMaybe <- runMaybeT maybeIOTest
    case resMaybe of
        Just () -> putStrLn "Just"
        Nothing -> putStrLn "Nothing"
    resWriterMaybe <- runMaybeT . runWriterT $ writerMaybeIOTest
    case resWriterMaybe of
        Just m -> putStrLn $ "Just " ++ show m
        Nothing -> putStrLn "Nothing"
    resMaybeWriter <- runWriterT . runMaybeT $ maybeWriterIOTest
    case resMaybeWriter of
        (Just  a, m) -> putStrLn $ "Just " ++ show a ++ ", "++ show m
        (Nothing, m) -> putStrLn $ "Nothing, " ++ show m
    return ()

main :: IO ()
main = testRun

