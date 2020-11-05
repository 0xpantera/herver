module FailAndBracket where

import Control.Exception.Safe (bracket)
import System.IO ( hShow, hClose, openFile, withFile, Handle, IOMode(WriteMode) )

getHandle :: String -> IO Handle
getHandle file = openFile file WriteMode

hShowHandle :: IO String
hShowHandle =
  do
    h <- getHandle "some_handle.txt"
    hShow h

showHandle :: IO String
showHandle =
  do
    h <- getHandle "some_other_handle.txt"
    pure $ show h

hShowHandleBracket :: IO String
hShowHandleBracket =
  bracket (openFile "some_handle.txt" WriteMode) hClose $ \h ->
    do
      hShow h

showHandleBracket :: IO String
showHandleBracket =
  bracket (openFile "some_handle.txt" WriteMode) hClose $ \h ->
    do
      pure $ show h

hShowHandleWithFile :: IO String
hShowHandleWithFile =
  withFile "some_handle.txt" WriteMode $ \h ->
    do
      hShow h

showHandleWithFile :: IO String
showHandleWithFile =
  withFile "some_handle.txt" WriteMode $ \h ->
    do
      pure $ show h


failBracket :: IO ()
failBracket = bracket open close use
  where
    open = putStrLn "open"
    close _ = putStrLn "close"
    use _ = putStrLn "use"

-- TODO
{--
exhaustion :: IO ()
exhaustion =
  bracket openManyHandles (traverse hClose) $ \hs ->
    putStrLn ("Opened " ++ show (length hs) ++ " handles")

openManyHandles :: IO [Handle]
openManyHandles = _
--}
