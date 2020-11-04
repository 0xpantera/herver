{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module Lib where


hello :: [Char] -> IO ()
hello s = putStrLn $ "Hey! " <> s