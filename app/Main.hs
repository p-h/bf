module Main where

import           Lib

import           System.Environment             ( getArgs )
import           System.IO                      ( BufferMode(..)
                                                , hSetBuffering
                                                , stdout
                                                , stdin
                                                )

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    (filename : _) <- getArgs
    file           <- readFile filename
    inp            <- getContents
    putStrLn $ run file inp
