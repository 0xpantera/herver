module Herver where

import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Data.Time as Time
import qualified System.IO as IO
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS

import Network.Socket (Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S

import Network.Simple.TCP (serve, HostPreference (..))
import qualified Network.Simple.TCP as Net

import ASCII (ASCII)
import ASCII.Decimal (Digit (..))
import qualified ASCII as A
import qualified ASCII.Char as A


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

line :: ByteString -> ByteString
line x = x <> A.fromCharList crlf

helloReqStr :: ByteString
helloReqStr =
    line [A.string|GET /hello.txt HTTP/1.1|] <>
    line [A.string|User-Agent: curl/7.64.1|] <>
    line [A.string|"Accept-Language: en, mi"|] <>
    line [A.string||]

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

helloResponseString :: ByteString
helloResponseString =
    line [A.string|HTTP/1.1 200 OK|] <>
    line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
    line [A.string|Content-Length: 6|] <>
    line [A.string||] <>
    [A.string|Hello!|]

fstServer :: IO a
fstServer = serve @IO HostAny "8000" \(s, a) -> do
    putStrLn ("New connection from " <> show a)
    Net.send s helloResponseString

-- Exercise 15 - Repeat until nothing
-- Define a function similar to 
-- repeatUntil :: Monad m => m chunk -> (chunk -> bool) -> (chunk -> m ()) -> m ()
-- using `Maybe` instead of a predicate to specify how far the reptition should go
repeatUntilNothing :: Monad m => m (Maybe chunk) -> (chunk -> m ()) -> m ()
repeatUntilNothing getChunk f = proceed
    where
        proceed = do
            chunk <- getChunk
            case chunk of
                Nothing -> pure ()
                Just chunk' -> do { f chunk'; proceed }

-- Exercise 16 - Make an HTTP request
mkHaskellWebReq :: IO ()
mkHaskellWebReq = runResourceT @IO do
    addrInfos <- liftIO $ resolve "http" "www.haskell.org"
    (_, s) <- openAndConnect addrInfos
    liftIO $ Net.send s haskellReqStr
    liftIO $ repeatUntilNothing (Net.recv s 1_024) BS.putStr


haskellReqStr :: ByteString
haskellReqStr =
    line [A.string|GET / HTTP/1.1|] <>
    line [A.string|Host: haskell.org|] <>
    line [A.string|Connection: close|] <>
    line [A.string||]    


data Request = Request RequestLine [Field] (Maybe Body)
data Response = Response StatusLine [Field] (Maybe Body)

data RequestLine = RequestLine Method RequestTarget Version
data Method = Method (ASCII ByteString)
data RequestTarget = RequestTarget (ASCII ByteString)
data Version = Version Digit Digit

data StatusLine = StatusLine Version StatusCode (Maybe ReasonPhrase)
data StatusCode = StatusCode Digit Digit Digit
data ReasonPhrase = ReasonPhrase (ASCII ByteString)

data Field = Field FieldName FieldValue
data FieldName = FieldName (ASCII ByteString)
data FieldValue = FieldValue (ASCII ByteString)

data Body = Body LByteString

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
    where
        start = RequestLine 
            (Method [A.string|GET|]) 
            (RequestTarget [A.string|/hello.txt|])
            (Version Digit1 Digit1)
        host = Field 
            (FieldName [A.string|Host|])
            (FieldValue [A.string|www.example.com|])
        lang = Field
            (FieldName [A.string|Accept-Language|])
            (FieldValue [A.string|en, mi|])

helloResponse :: Response
helloResponse = Response start [typ, len] (Just body)
    where
        start = StatusLine
            (Version Digit1 Digit1)
            (StatusCode Digit2 Digit0 Digit0)
            (Just $ ReasonPhrase [A.string|OK|])
        typ = Field
            (FieldName [A.string|Content-Type|])
            (FieldValue [A.string|text/plain; charset=us-ascii|])
        len = Field
            (FieldName [A.string|Content-Length|])
            (FieldValue [A.string|6|])
        body = Body [A.string|Hello!|]

sayHelloBuilder :: Text -> Text
sayHelloBuilder name = LT.toStrict $ TB.toLazyText $
    TB.fromString "Hello " <> TB.fromText name <> TB.fromString "!"

time :: IO () -> IO ()
time action = do
    a <- Time.getCurrentTime
    action
    b <- Time.getCurrentTime
    print (Time.diffUTCTime b a)

concatWithStrict :: Int -> Text
concatWithStrict nTimes = fold $ replicate nTimes $ T.pack "a"

concatWithBuilder :: Int -> Text
concatWithBuilder nTimes = LT.toStrict $ TB.toLazyText $
    fold $ replicate nTimes $ TB.fromString "a"

concatSpeedTest :: Int -> IO ()
concatSpeedTest n = do
    dir <- getDataDir
    time $ T.writeFile (dir </> "strict.txt") (concatWithStrict n)
    time $ T.writeFile (dir </> "builder.txt") (concatWithBuilder n)

encodeLineEnd :: BSB.Builder
encodeLineEnd = A.fromCharList crlf

encodeRequest :: Request -> BSB.Builder
encodeRequest (Request reqLine fields bodyMaybe) =
    encodeRequestLine reqLine
    <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
    <> encodeLineEnd
    <> optionallyEncode encodeBody bodyMaybe

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statLine fields bodyMaybe) =
    encodeStatusLine statLine    
    <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
    <> encodeLineEnd
    <> optionallyEncode encodeBody bodyMaybe

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
repeatedlyEncode = foldMap

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode = foldMap 

encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method target version) =
    encodeMethod method <> A.fromCharList [A.Space]
    <> encodeRequestTarget target <> A.fromCharList [A.Space]
    <> encodeVersion version <> encodeLineEnd

encodeMethod :: Method -> BSB.Builder
encodeMethod (Method x) = BSB.byteString (A.lift x)

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget (RequestTarget x) = BSB.byteString (A.lift x)

encodeVersion :: Version -> BSB.Builder
encodeVersion (Version x y) =
    [A.string|HTTP/|] <> A.fromDigitList [x] <> [A.string|.|] <> A.fromDigitList [y]

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine version code reason) =
    encodeVersion version <> A.fromCharList [A.Space] <> encodeStatusCode code <>
    A.fromCharList [A.Space] <> optionallyEncode encodeReasonPhrase reason <>
    encodeLineEnd

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) = A.fromDigitList [x, y, z]

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase x) = BSB.byteString (A.lift x)

encodeField :: Field -> BSB.Builder
encodeField (Field (FieldName x) (FieldValue y)) =
    BSB.byteString (A.lift x) <> A.fromCharList [A.Colon, A.Space] <> BSB.byteString (A.lift y)

encodeBody :: Body -> BSB.Builder
encodeBody (Body x) = BSB.lazyByteString x