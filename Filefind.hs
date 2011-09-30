module Main (main) where

import Control.Exception
import Control.Monad
import System.Environment     (getArgs)
import System.FilePath
import System.Posix.Directory
import System.Posix.Files

recurse :: (FilePath -> IO ()) -> FilePath -> IO ()
recurse action dir = bracket (openDirStream dir) closeDirStream go
    where
      go stream = do
        name <- readDirStream stream
        case name of
          [] -> return ()
          _ -> do let path = dir </> name
                  case name of
                    "." -> return ()
                    ".." -> return ()
                    _ -> do action path
                            stat <- getFileStatus path
                            when (isDirectory stat) $ recurse action path
                  go stream

main :: IO ()
main = do
  args <- getArgs
  let dir = case args of
              [] -> "."
              (x:_) -> x
  recurse putStrLn dir
