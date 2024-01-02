module Herver where

import Relude

import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS

import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Directory as Dir
import Network.Socket (Socket)

import Network.Simple.TCP (serve, HostPreference (..))
import qualified Network.Simple.TCP as Net

import ASCII (ASCII)
import ASCII.Decimal (Digit (..))
import qualified ASCII as A
import qualified ASCII.Char as A

import qualified Control.Concurrent.Async as Async
import qualified Data.Time as Time


getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    pure dir

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose


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
            unless (isEnd chunk) do
                f chunk
                proceed

line :: ByteString -> ByteString
line x = x <> A.fromCharList crlf

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]


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

data Status = Status StatusCode (Maybe ReasonPhrase)

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

encodeFieldList :: [Field] -> BSB.Builder
encodeFieldList xs =
    repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) xs
    <> encodeLineEnd    

encodeBody :: Body -> BSB.Builder
encodeBody (Body x) = BSB.lazyByteString x

countHelloAscii :: Natural -> ASCII LByteString
countHelloAscii count = [A.string|Hello!|] <> A.fromCharList crlf <> case count of
    0 -> [A.string|This page has never been viewed|]
    1 -> [A.string|This page has been viewed 1 time.|]
    _ -> [A.string|This page has been viewed |] <>
        A.showIntegralDecimal count <> [A.string| times.|]

ok :: Status
ok = Status (StatusCode Digit2 Digit0 Digit0) (Just (ReasonPhrase [A.string|OK|]))

status :: Status -> StatusLine
status (Status code phrase) = StatusLine http_1_1 code phrase

http_1_1 :: Version
http_1_1 = Version Digit1 Digit1

contentType :: FieldName
contentType = FieldName [A.string|Content-Type|]

plainAscii :: FieldValue
plainAscii = FieldValue [A.string|text/plain; charset=us-ascii|]

contentLength :: FieldName
contentLength = FieldName [A.string|Content-Length|]

asciiOk :: ASCII LByteString -> Response
asciiOk str = Response (status ok) [typ, len] (Just body)
    where
        typ = Field contentType plainAscii
        len = Field contentLength (bodyLengthValue body)
        body = Body (A.lift str)

bodyLengthValue :: Body -> FieldValue
bodyLengthValue (Body x) = FieldValue (A.showIntegralDecimal (LBS.length x))

sendResponse :: Socket -> Response -> IO ()
sendResponse s r = Net.sendLazy s $ BSB.toLazyByteString (encodeResponse r)

plainUtf8 :: FieldValue
plainUtf8 = FieldValue [A.string|text/plain; charset=utf-8|]

htmlUtf8 :: FieldValue
htmlUtf8 = FieldValue [A.string|text/html; charset=utf-8|]

json :: FieldValue
json = FieldValue [A.string|application/json|]

countHelloText :: Natural -> LText
countHelloText count = TB.toLazyText $
    TB.fromString "Hello \9835\r\n" <>
    case count of
        0 -> TB.fromString "This page has never been viewed."
        1 -> TB.fromString "This page has been viewed 1 time."
        _ -> TB.fromString "This page has been viewed " <>
             TB.decimal count <> TB.fromString " times."

textOk :: LText -> Response
textOk str = Response (status ok) [typ, len] (Just body)
    where
        typ = Field contentType plainUtf8
        len = Field contentLength (bodyLengthValue body)
        body = Body (LT.encodeUtf8 str)


stuckCountingServerText :: IO ()
stuckCountingServerText = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0
    sendResponse s (textOk (countHelloText count))

stuckCountingServer :: IO ()
stuckCountingServer = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0 --to-do
    sendResponse s (asciiOk (countHelloAscii count))    

countingServer :: IO ()
countingServer = do
    hitCounter <- atomically (newTVar @Natural 0)
    serve @IO HostAny "8000" \(s, _) -> do
        count <- atomically $ increment hitCounter
        sendResponse s (textOk (countHelloText count))

increment :: TVar Natural -> STM Natural
increment hitCounter = do
    oldCount <- readTVar hitCounter
    let newCount = oldCount + 1
    writeTVar hitCounter newCount
    return newCount

incrementNotAtomic :: TVar Natural -> IO Natural
incrementNotAtomic x = do
    oldCount <- readTVarIO x
    return (oldCount + 1)

testIncrement :: (TVar Natural -> IO a) -> IO Natural
testIncrement inc = do
    x <- atomically (newTVar @Natural 0)
    Async.replicateConcurrently_ 10 (replicateM 1_000 (inc x))
    atomically (readTVar x)

timingServer :: IO ()
timingServer = do
    timeVar <- atomically $ newTVar @(Maybe Time.UTCTime) Nothing
    serve @IO HostAny "8000" \(s, _) -> do
        now <- Time.getCurrentTime
        diff <- atomically $ updateTime timeVar now
        sendResponse s $ textOk $ show diff

updateTime ::
    TVar (Maybe Time.UTCTime)
    -> Time.UTCTime
    -> STM (Maybe Time.NominalDiffTime)
updateTime timeVar now = do
    previousTimeMaybe <- readTVar timeVar
    writeTVar timeVar (Just now)
    return $ Time.diffUTCTime now <$> previousTimeMaybe

data Chunk = Chunk ChunkSize ChunkData

data ChunkSize = ChunkSize Natural

data ChunkData = ChunkData ByteString

transferEncoding :: FieldName
transferEncoding = FieldName [A.string|Transfer-Encoding|]

chunked :: FieldValue
chunked = FieldValue [A.string|chunked|]

transferEncodingChunked :: Field
transferEncodingChunked = Field transferEncoding chunked

dataChunk :: ChunkData -> Chunk
dataChunk chunkData = Chunk (chunkDataSize chunkData) chunkData

chunkDataSize :: ChunkData -> ChunkSize
chunkDataSize (ChunkData bs) = case toIntegralSized @Int @Natural (BS.length bs) of
    Just n -> ChunkSize n
    Nothing -> error (T.pack "BS.length is always Natural")


encodeChunk :: Chunk -> BSB.Builder
encodeChunk (Chunk chunkSize chunkData) =
    encodeChunkSize chunkSize <> encodeLineEnd <>
    encodeChunkData chunkData <> encodeLineEnd

encodeChunkSize :: ChunkSize -> BSB.Builder
encodeChunkSize (ChunkSize x) = A.showIntegralHexadecimal A.LowerCase x

encodeLastChunk :: BSB.Builder
encodeLastChunk = encodeChunkSize (ChunkSize 0) <> encodeLineEnd

encodeChunkData :: ChunkData -> BSB.Builder
encodeChunkData (ChunkData x) = BSB.byteString x

fileStreaming :: IO ()
fileStreaming = do
    dir <- getDataDir
    serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
        (_, h) <- binaryFileResource (dir </> "stream.txt") ReadMode
        liftIO do
            sendBSB s (encodeStatusLine (status ok))
            sendBSB s (encodeFieldList [transferEncodingChunked])
            repeatUntil (BS.hGetSome h 1_024) BS.null \c ->
                sendBSB s (encodeChunk (dataChunk (ChunkData c)))
            sendBSB s encodeLastChunk
            sendBSB s (encodeFieldList [])


sendBSB :: Socket -> BSB.Builder -> IO ()
sendBSB s bs = Net.sendLazy s (BSB.toLazyByteString bs)                