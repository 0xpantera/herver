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
