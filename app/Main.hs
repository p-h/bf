module Main where

import           Lib

import           System.Environment             ( getArgs )

main :: IO ()
main = do
    (filename : _) <- getArgs
    file           <- readFile filename
    eval file
