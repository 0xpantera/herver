module Main where

import Lib
import Control.Exception.Safe (bracket)
import System.IO ( hClose, openFile, hPutStrLn, withFile, IOMode (..) )



main :: IO ()
main = do
  h <- openFile "greeting.txt" WriteMode
  hPutStrLn h "hello"
  hPutStrLn h "world"
  hClose h

main2 :: IO ()
main2 = 
    bracket (openFile "greetings.txt" WriteMode) hClose $ \h ->
        do
            hPutStrLn h "hello"
            hPutStrLn h "world"

main3 :: IO ()
main3 =
    withFile "greetings.txt" WriteMode $ \h ->
        do
            hPutStrLn h "hello"
            hPutStrLn h "world"
