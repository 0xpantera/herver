module Herver where

import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified System.IO as IO
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)


getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    pure dir

writeGreetingFile :: IO ()
writeGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- fileResource (dir </> "greeting.txt") WriteMode
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")


-- Exercise 1 - File resource function
fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose

-- Exercise 2 - Showing handles
handlePrintTest :: IO ()
handlePrintTest = runResourceT @IO do
    (_, fh1) <- fileResource "/tmp/handle-print-test-1.txt" WriteMode
    (_, fh2) <- fileResource "/tmp/handle-print-test-2.txt" ReadWriteMode
    liftIO $ for_ [stdin, stdout, stderr, fh1, fh2] \h -> do
        IO.putStrLn (show h)
        info <- IO.hShow h
        IO.putStrLn info
        IO.putStrLn ""

-- Exercise 3 - Exhaustion
howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
    hs <- openManyHandles
    putStrLn ("Opened " <> show (length hs) <> " handles")

openManyHandles :: ResourceT IO [Handle]
openManyHandles = go []
    where
        go hs = do
            r <- fileResourceMaybe
            case r of
                Nothing -> return hs
                Just h -> go (h : hs)

fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
    dir <- liftIO getDataDir
    result <- tryAny do
        (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
        pure $ Just h
    case result of
        Right x -> return x
        Left e -> do
            print (displayException e)
            return Nothing
