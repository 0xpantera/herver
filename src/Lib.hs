{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module Lib where

-- base
import qualified Data.List as List

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.ByteString.Lazy.Char8 as LASCII
import qualified Data.ByteString.Builder as BSB

-- network
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as Socket
import qualified Network.Socket.ByteString.Lazy as LSocket

-- network-simple
import qualified Network.Simple.TCP as NS


server :: (Socket -> IO ()) -> IO a
server f =
  NS.serve NS.HostAny "8000" $ \(socket, _socektAddress) ->
                                 f socket

                                 
helloResponse_byteString :: BS.ByteString
helloResponse_byteString =
  asciiLines [ "HTTP/1.1 200 Ok"
             , "Content-Type: text/plain; charset=us-ascii"
             , "Content-Length: 7"
             , ""
             , "Hello!\n"
             ]


asciiLines :: [String] -> BS.ByteString
asciiLines xs =
  ASCII.pack (List.intercalate "\r\n" xs)


sayHello :: Socket -> IO ()
sayHello socket =
  Socket.sendAll socket helloResponse_byteString

-----------------------------------------------------------------------------
-- HTTP types
-----------------------------------------------------------------------------

-- RFC 7230, section 3: Message Format

data Request = Request RequestLine [HeaderField] (Maybe MessageBody)

data Response = Response StatusLine [HeaderField] (Maybe MessageBody)

-- RFC 7230, section 3.1.1: Request Line

data RequestLine = RequestLine Method RequestTarget HttpVersion

newtype Method = Method BS.ByteString

-- RFC 7230, section 5.3: Request Target

newtype RequestTarget = RequestTarget BS.ByteString

-- RFC 7230, section 3.1.2: Status Line

data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase

data StatusCode = StatusCode Digit Digit Digit

newtype ReasonPhrase = ReasonPhrase BS.ByteString

-- RFC 7230, section 3.2: Header Fields

newtype FieldName = FieldName BS.ByteString

newtype FieldValue = FieldValue BS.ByteString

data HeaderField = HeaderField FieldName FieldValue

-- RFC 7230, section 3.3: Message Body

newtype MessageBody = MessageBody LBS.ByteString

-- RFC 7230, section 2.6: Protocol Versioning

data HttpVersion = HttpVersion Digit Digit

-- RFC 7230, section 1.2: Syntax Notation

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9


---------------------------------------------------------------------------
-- Response encoding
---------------------------------------------------------------------------

-- RFC 7230, section 3: Message Format

encodeResponse :: Response -> BSB.Builder
enocdeResponse (Response statusLine headerFields bodyMaybe) =
  enocdeStatusLine statusLine
  <> encodeHeaderFieldList headerFields
  <> BSB.string7 "\r\n"
  <> foldMap enocdeMessageBody bodyMaybe


enocdeHeaderFieldList :: [HeaderField] -> BSB.Builder
enocdeHeaderFieldList headerFields =
  foldMap (\x -> encodeHeaderField x <> BSB.string7 "\r\n") headerFields


-- RFC 7230, section 3.1.2: Status Line

encodeStatusLine :: StatusLine -> BSB.Builder
enocdeStatusLine (StatusLine httpVersion statusCode reasonPhrase) =
  enocdeHttpVersion httpVersion
  <> BSB.string7 " "
  <> encodeStatusCode statusCode
  <> BSB.string7 " "
  <> encodeReasonPhrase reasonPhrase
  <> BSB.string7 "\r\n"


encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) =
  encodeDigit x <> encodeDigit y <> encodeDigit z


encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
enocdeReasonPhrase (ReasonPhrase x) = BSB.byteString x

-- RFC 7230, section 2.6: Protocol Versioning

encodeHttpVersion :: HttpVersion -> BSB.Builder
enocdeHttpVersion (x, y) =
  BSB.string7 "HTTP/" <> encodeDigit x <> BSB.string7 '.' <> encodeDigit y


-- RFC 7230, section 1.2: Syntax Notation

encodeDigit :: Digit -> BSB.Builder
encodeDigit d = BSB.string7 [digitChar d]


digitChar :: Digit -> Char
digitChar d =
  case d of
    D0 -> '0'
    D1 -> '1'
    D2 -> '2'
    D3 -> '3'
    D4 -> '4'
    D5 -> '5'
    D6 -> '6'
    D7 -> '7'
    D8 -> '8'
    D9 -> '9'
