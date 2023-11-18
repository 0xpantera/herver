module Herver where

import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))

getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.CreateDirectoryIfMissing True dir
    return dir