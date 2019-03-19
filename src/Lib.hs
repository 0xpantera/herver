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
encodeResponse (Response statusLine headerFields bodyMaybe) =
  encodeStatusLine statusLine
  <> encodeHeaderFieldList headerFields
  <> BSB.string7 "\r\n"
  <> foldMap encodeMessageBody bodyMaybe


encodeHeaderFieldList :: [HeaderField] -> BSB.Builder
encodeHeaderFieldList headerFields =
  foldMap (\x -> encodeHeaderField x <> BSB.string7 "\r\n") headerFields


-- RFC 7230, section 3.1.2: Status Line

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine httpVersion statusCode reasonPhrase) =
  encodeHttpVersion httpVersion
  <> BSB.string7 " "
  <> encodeStatusCode statusCode
  <> BSB.string7 " "
  <> encodeReasonPhrase reasonPhrase
  <> BSB.string7 "\r\n"


encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) =
  encodeDigit x <> encodeDigit y <> encodeDigit z


encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase x) = BSB.byteString x

-- RFC 7230, section 2.6: Protocol Versioning

encodeHttpVersion :: HttpVersion -> BSB.Builder
encodeHttpVersion (HttpVersion x y) =
  BSB.string7 "HTTP/"
  <> encodeDigit x
  <> BSB.string7 "."
  <> encodeDigit y


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

-- RFC 7230, section 3.2: Header Fields

encodeHeaderField :: HeaderField -> BSB.Builder
encodeHeaderField
  (HeaderField (FieldName x) (FieldValue y)) =
  BSB.byteString x <> BSB.string7 ": " <> BSB.byteString y


encodeMessageBody :: MessageBody -> BSB.Builder
encodeMessageBody (MessageBody x) = BSB.lazyByteString x


-------------------------------------------------------------------------
-- Construct and use Responses
-------------------------------------------------------------------------


server :: (Socket -> IO ()) -> IO a
server f =
  NS.serve NS.HostAny "8000" $ \(socket, _socektAddress) ->
                                 f socket


staticResponseServer :: Response -> IO ()
staticResponseServer response =
  server $ \socket ->
             LSocket.sendAll socket
             (BSB.toLazyByteString
              (encodeResponse response))


helloResponse_withMoreTypes :: Response
helloResponse_withMoreTypes = Response statusLine [hf1, hf2] (Just messageBody)
  where
    statusLine   = StatusLine httpVersion statusCode reasonPhrase
    httpVersion  = HttpVersion D1 D1
    statusCode   = StatusCode D2 D0 D0
    reasonPhrase = ReasonPhrase $ ASCII.pack "OK"
    hf1           = HeaderField
                       (FieldName $ ASCII.pack "Content-Type")
                       (FieldValue $ ASCII.pack "text/plain; charset=us-ascii")
    hf2          = HeaderField
                       (FieldName $ ASCII.pack "Content-Length")
                       (FieldValue $ ASCII.pack "7")
    messageBody  = MessageBody $ LASCII.pack "Hello!\n"
                  
                  
                  
        









--  Response
-- (StatusLine
--      (HttpVersion Digit 1 Digit 1)
--      (StatusCode Digit 2 Digit 0 Digit 0)
--      ReasonPhrase BSB.byteString "OK")
--  [(HeaderField
--      FieldName BSB.byteString "Content-Type"
--      FieldValue BSB.byteString "text/plain; charset=us-ascii")
--   (HeaderField
--      FieldName BSB.byteString "Content-Length"
--      FieldValue BSB.byteString "7")]
--   Just $ MessageBody "Hello!\n"
