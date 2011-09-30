{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Exception
import Control.Monad
import System.Environment     (getArgs)
import System.FilePath
import System.Posix.Directory
import System.Posix.Files
import Prelude hiding (negate)

data Positivity
    = Positive
    | Negative
    deriving (Show)

applyPositivity :: Positivity -> Bool -> Bool
applyPositivity Positive = id
applyPositivity Negative = not

data FileInfo
    = FileInfo {
        fiName :: FilePath
      , fiPath :: FilePath
      , fiStatus :: FileStatus
      }

type Test = Positivity -> FileInfo -> TestResult

data TestResult 
    = TestResult {
        trAction :: Bool
      , trDescend :: Maybe Test
      }

allContents :: Test
allContents Positive _fi = TestResult { trAction = True, trDescend = Just allContents }
allContents Negative _fi = TestResult { trAction = False, trDescend = Nothing }

leafTest :: (FileInfo -> Bool) -> Test
leafTest tst = t
    where t Positive fi = case tst fi of
                             True -> TestResult { trAction = True, trDescend = Just t }
                             False -> TestResult { trAction = False, trDescend = Just t }
          t Negative fi = case tst fi of
                             False -> TestResult { trAction = True, trDescend = Just nt }
                             True -> TestResult { trAction = False, trDescend = Just nt }
          nt = negate t

-- TODO: use glob
testName :: FilePath -> Test
testName x = leafTest $ \FileInfo{fiName=n} -> n == x

negate :: Test -> Test
negate t Positive = t Negative
negate t Negative = t Positive

recurse :: (FilePath -> IO ()) -> Test -> FilePath -> IO ()
recurse action test dir = bracket (openDirStream dir) closeDirStream go
    where
      go stream = do
        name <- readDirStream stream
        case name of
          [] -> return ()
          _ -> do case name of
                    "." -> return ()
                    ".." -> return ()
                    _ -> do let path = dir </> name
                            stat <- getFileStatus path
                            let fi = FileInfo { fiName = name, fiPath = path, fiStatus = stat }
                            let TestResult{..} = test Positive fi
                            when trAction $ action path
                            when (isDirectory stat) $ case trDescend of
                              Just nextTest -> recurse action nextTest path
                              Nothing -> return ()
                  go stream

main :: IO ()
main = do
  args <- getArgs
  let dir = case args of
              [] -> "."
              (x:_) -> x
  recurse putStrLn (negate $ testName "x") dir
