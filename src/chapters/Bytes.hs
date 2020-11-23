module Chapters.Bytes where

import System.IO
import qualified Data.ByteString as BS
import Chapters.Chunks ( forChunks_ )

readWrite =
    withBinaryFile "greeting.txt" ReadMode $ \h1 ->
        withBinaryFile "greeting-2.txt" WriteMode $ \h2 ->
            forChunks_ (BS.hGetSome h1 1024) BS.null $ \chunk ->
                BS.hPutStr h2 chunk