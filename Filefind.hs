{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Applicative    hiding (many)
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Prelude                hiding (negate)
import           System.Environment     (getArgs)
import           System.FilePath
import           System.Posix.Directory
import           System.Posix.Files
import           Text.Parsec            hiding ((<|>))
import qualified Data.Map               as M

type Variables = M.Map String String

type Parser a = Parsec String Variables a

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

noContents :: Test
noContents = negate allContents

leafTest :: (FileInfo -> Bool) -> Test
leafTest tst = t
    where t Positive fi = case tst fi of
                             True -> TestResult { trAction = True, trDescend = Just t }
                             False -> TestResult { trAction = False, trDescend = Just t }
          t Negative fi = case tst fi of
                             True -> TestResult { trAction = False, trDescend = Just nt }
                             False -> TestResult { trAction = True, trDescend = Just nt }
          nt = negate t

-- TODO: use glob
testName :: FilePath -> Test
testName x = leafTest $ \FileInfo{fiName=n} -> n == x

negate :: Test -> Test
negate t Positive = t Negative
negate t Negative = t Positive

cut :: Test -> Test
cut t Positive fi = case t Positive fi of
                      TestResult{trAction=True} -> TestResult{trAction=True, trDescend=Nothing}
                      TestResult{trAction=False,trDescend=d} -> TestResult{trAction=False, trDescend=fmap cut d}
cut t Negative fi = case t Positive fi of
                      TestResult{trAction=True} -> TestResult{trAction=False, trDescend=Just allContents}
                      TestResult{trAction=False,trDescend=d} -> TestResult{trAction=True, trDescend=fmap (negate . cut) d}

intersection :: Test -> Test -> Test
intersection a b Positive = \fi -> let TestResult{trAction=a1,trDescend=d1} = a Positive fi
                                       TestResult{trAction=a2,trDescend=d2} = b Positive fi
                                   in TestResult{trAction=a1&&a2, trDescend=liftM2 intersection d1 d2}
intersection a b Negative = (negate a `union` negate b) Positive

-- lolwut
compose :: Test -> Test -> Test
compose = intersection

union :: Test -> Test -> Test
union a b Positive = \fi -> let TestResult{trAction=a1,trDescend=d1} = a Positive fi
                                TestResult{trAction=a2,trDescend=d2} = b Positive fi
                            in TestResult{
                                     trAction  = a1 || a2
                                   , trDescend = case (d1,d2) of
                                                   (Nothing,Nothing) -> Nothing
                                                   (_,Nothing) -> d1
                                                   (Nothing,_) -> d2
                                                   (Just x, Just y) -> Just (x `union` y)
                                   }
union a b Negative = (negate a `intersection` negate b) Positive

difference :: Test -> Test -> Test
difference a b = a `intersection` negate b

makeFileInfo :: FilePath -> IO FileInfo
makeFileInfo path = makeFileInfo' (takeFileName path) path

makeFileInfo' :: FilePath -> FilePath -> IO FileInfo
makeFileInfo' name path = do
  stat <- getFileStatus path
  return FileInfo{fiName=name, fiPath=path, fiStatus=stat}

traverse :: (FilePath -> IO ()) -> Test -> FileInfo -> IO ()
traverse action test fi@FileInfo{fiPath=path}
    = do let TestResult{..} = test Positive fi
         when trAction $ action path
         when (isDirectory $ fiStatus fi) $ 
              case trDescend of
                Just nextTest -> bracket (openDirStream path) closeDirStream (go nextTest)
                Nothing -> return ()
    where
      go nextTest stream = do
        name <- readDirStream stream
        case name of
          [] -> return ()
          _ -> do case name of
                    "." -> return ()
                    ".." -> return ()
                    _ -> traverse action nextTest =<< makeFileInfo' name (path </> name)
                  go nextTest stream

testWithNegation :: Test -> FilePath -> IO ()
testWithNegation t dir = do
  fi <- makeFileInfo dir
  putStrLn "Positive:"
  traverse putStrLn t fi
  putStrLn "\nNegative:"
  traverse putStrLn (negate t) fi

quote :: Parser String
quote = xquote '"' <|> xquote '\''
    where xquote q = between (char q) (char q) innerQuote <?> "quote <" ++ q : ">"
              where innerQuote = concat <$> many ((escaped <|> interpolated <|> normal) <?> "text")
                    escaped = do char '~'
                                 c <- oneOf "$~'\"0n"
                                 return $ case c of
                                            '0' -> "\0"
                                            'n' -> "\n"
                                            _   -> [c]
                    normal = (:[]) <$> noneOf [q]
                    interpolated = do char '$'
                                      name <- between (char '{') (char '}') varname <|> varname
                                      findVar name

bareword :: Parser String
bareword = many1 (oneOf $ "./-" ++ filter (\c -> isLetter c || isNumber c) [minBound..maxBound]) <?> "bareword"

variable :: Parser String
variable = do char '$'
              findVar =<< varname

findVar :: String -> Parser String
findVar name = do vars <- getState
                  case M.lookup name vars of
                    Nothing -> fail ("Unknown variable " ++ name)
                    Just x -> return x

varname :: Parser String
varname = (:) <$> oneOf ('_' : ['A'..'Z']++['a'..'z']) <*> many (oneOf $ '_' : ['A'..'Z']++['a'..'z']++['0'..'9'])

word :: Parser String
word = (variable <|> quote <|> bareword) <* spaces

expression :: Parser Test
expression = chainl factor expop allContents
    where
      expop = (<* spaces) $ union <$ char '+'
                            <|> intersection <$ char '&'
                            <|> difference <$ oneOf "%\\"
      factor = name <|> between (char '(' <* spaces) (char ')' <* spaces) expression
      name = testName <$> word

main :: IO ()
main = do
  args <- getArgs
  let dir = case args of
              [] -> "."
              (x:_) -> x
  testWithNegation (testName "x" `difference` cut (testName "x")) dir
