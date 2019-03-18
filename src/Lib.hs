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
