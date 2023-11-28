module Herver where

import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified System.IO as IO
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Char as Char
import qualified Data.ByteString as BS

import Network.Socket (Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S


getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    pure dir

writeGreetingFile :: IO ()
writeGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- fileResource (dir </> "greeting.txt") WriteMode
    liftIO (T.hPutStrLn h $ T.pack "hello")
    liftIO (T.hPutStrLn h $ T.pack "world")


-- Exercise 1 - File resource function
fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose

-- Exercise 2 - Showing handles
handlePrintTest :: IO ()
handlePrintTest = runResourceT @IO do
    (_, fh1) <- fileResource "/tmp/handle-print-test-1.txt" WriteMode
    (_, fh2) <- fileResource "/tmp/handle-print-test-2.txt" ReadWriteMode
    liftIO $ for_ [stdin, stdout, stderr, fh1, fh2] \h -> do
        T.putStrLn (show h)
        info <- IO.hShow h
        T.putStrLn $ T.pack info
        T.putStrLn $ T.pack ""

-- Exercise 3 - Exhaustion
howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
    hs <- openManyHandles
    let handleTxt = T.pack ("Opened " <> show (length hs) <> " handles")
    liftIO $ T.putStrLn handleTxt

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


printFileContentsUpperCase :: IO ()
printFileContentsUpperCase = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    liftIO (printCapitalizedText h)

printCapitalizedText :: Handle -> IO ()
printCapitalizedText h = proceed
    where
        proceed = do
            chunk <- T.hGetChunk h
            if T.null chunk
            then return ()
            else do { T.putStr (T.toUpper chunk); proceed}


repeatUntilIO ::
    IO chunk
    -> (chunk -> Bool)
    -> (chunk -> IO ())
    -> IO ()
repeatUntilIO getChunk isEnd f = proceed
    where
        proceed = do
            chunk <- getChunk
            if isEnd chunk
            then return ()
            else do { f chunk; proceed }

printFileContentsUpperCase' :: IO ()
printFileContentsUpperCase' = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greetings.txt") ReadMode
    liftIO $ repeatUntilIO (T.hGetChunk h) T.null (T.putStr . T.toUpper)

-- Exercise 4 - Find the numbers
-- Get only the numeric characters from the text
digitsOnly :: Text -> Text
digitsOnly = T.filter Char.isNumber

-- Exercise 5 - Capitalize the last
-- Capitalize the last letter of a Text value
capitalizeLast :: Text -> Text
capitalizeLast txt = 
    case T.unsnoc txt of
        Nothing -> T.empty
        Just (body, lastLetter) -> T.snoc body (Char.toUpper lastLetter)

-- Exercise 6 - Paren removal
-- remove one layer of parentheses from text if possible
-- otherwise return `Nothing`
unParen :: Text -> Maybe Text
unParen txt = 
    case T.stripPrefix (T.pack "(") txt of
        Nothing -> Nothing
        Just t' -> T.stripSuffix (T.pack ")") t'

-- Exercise 7 - Character Count
-- the following prints the number of characters in a file
characterCount :: FilePath -> IO Int
characterCount fp = do
    dir <- getDataDir
    x <- T.readFile (dir </> fp)
    return $ T.length x

-- The problem is that T.readFile reads the entire file into memory
-- Fix this by rewriting it as a recursive loop over smaller chunks
-- of text read from a Handle
characterCount' :: FilePath -> IO Int
characterCount' fp = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> fp) ReadMode
    liftIO $ hCharacterCount h

hCharacterCount :: Handle -> IO Int
hCharacterCount h = proceed 0
    where
        proceed n = do
            x <- T.hGetChunk h
            if T.null x
            then pure n
            else proceed (n + T.length x)


-- Exercise 8 - Beyond IO
-- Define a function repeatUntil that generalizes repeatUntilIO
-- to work with any monadic type constructor
repeatUntil :: Monad m => 
    m chunk 
    -> (chunk -> Bool) 
    -> (chunk -> m ()) 
    -> m ()
repeatUntil getChunk isEnd f = proceed
    where
        proceed = do
            chunk <- getChunk
            unless' (isEnd chunk) do
                f chunk
                proceed 


-- Exercise 9 - When and unless
-- In the Control.Monad module, you can find these two functions:
-- when   :: Bool -> IO () -> IO ()
-- unless :: Bool -> IO () -> IO ()
-- We use these functions to perform an action conditionally. 
-- For when, the action is per- formed only when the Bool is True. 
-- When the Bool is False, nothing happens. unless is the opposite: 
-- The action is performed only when the Bool is False. 
-- In other words, the action is performed unless the Bool is True.
-- Modify the definition of the repeatUntil function to make use of either when or unless.
-- Write your own definitions of when and unless instead of using the library definitions.
when' :: Monad m => Bool -> m () -> m ()
when' b eff = do
    if b
    then eff
    else return ()

unless' :: Monad m => Bool -> m () -> m ()
unless' b eff = do
    if b
    then return ()
    else eff

exampleBytes :: [Word8]
exampleBytes = [104, 101, 108, 108, 111]



copyGreetingFile :: IO ()
copyGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
    (_, h2) <- binaryFileResource (dir </> "greeting-copy.txt") WriteMode
    liftIO $ repeatUntil (BS.hGetSome h1 1_024) BS.null \chunk -> BS.hPutStr h2 chunk


binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

helloByteString :: IO ()
helloByteString = do
    IO.hSetBinaryMode stdout True
    BS.hPut stdout (BS.pack helloBytes)
    where
        helloBytes = [
            104, 101, 108, 108, 111,     -- hello
            32,                          -- space
            119, 111, 114, 108, 100, 33, -- world!
            10 ]                         -- \n'

helloUtf8 :: IO ()
helloUtf8 = do
    IO.hSetBinaryMode stdout True
    BS.hPutStr stdout (encodeUtf8 (T.pack "hello world!\n"))

-- Exercise 10 - A character encoding bug
-- The following function accepts the UTF-8 encoding of
-- a person's name and prints a greeting for that person.
greet :: ByteString -> IO ()
greet nameBS = case decodeUtf8' nameBS of
    Left _ -> putStrLn "Invalid byte string"
    Right nameTxt -> T.putStrLn (T.pack "Hello, " <> nameTxt)

-- Exercise 11 - Byte manipulation
-- If we study how a particular character encoding works,
-- it might be advantageous to a program's speed to write text
-- manipulation functions that operate directly over the bytes
-- in a ByteString.
-- Write a function that converts any lowercase characters in
-- ASCII-encoded string to uppercase.
asciiUpper :: ByteString -> ByteString
asciiUpper = BS.map \x -> if (x >= 97 && x <= 122) then (x - 32) else x

mkFriendAddrInfo :: S.AddrInfo -> IO ()
mkFriendAddrInfo addressInfo = runResourceT @IO do
    (_, s) <- allocate (S.openSocket addressInfo) S.close
    liftIO do
        --S.setSocketOption s S.UserTimeout 1_000 unsupported on MacOS
        S.connect s (S.addrAddress addressInfo)
        S.sendAll s $ encodeUtf8 $ T.pack "Hello, will you be my friend?"
        repeatUntil (S.recv s 1_024) BS.null BS.putStr
        S.gracefulClose s 1_000

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just "www.haskell.org")
        (Just "http")
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x        

-- Exercise 12 - Improper ResourceT allocation
-- Since the `connect` step is part of the initial socket that
-- we have to get out of the way before we do anything else with the socket,
-- we might want to combine `openSocket` and `connect` into a single function.
openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect addrInfo = do
    (rk, s) <- allocate (S.openSocket addrInfo) S.close
    liftIO do
        S.connect s (S.addrAddress addrInfo)
    pure (rk, s)

-- Exercise 14 - Address resolution
-- In networking parlance, looking up an address is described as "resolving" an
-- address. Generalize `findHaskellWebsite` to implement the following function
resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve serviceName hostName = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just hostName)
        (Just serviceName)
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x