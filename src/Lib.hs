module Lib
    ( entryPoint
    )
where

import           Text.ParserCombinators.Parsec
import           System.IO
import           System.IO.Error
import           Control.Exception
import           System.Environment
import           Jian

entryPoint :: IO ()
entryPoint = getFile `catch` handler

getFile :: IO ()
getFile = do
    (name : _) <- getArgs
    handle     <- openFile name ReadMode
    hSetEncoding handle utf8
    contents <- hGetContents handle
    writeFile (takeWhile (/= '.') name ++ ".md") $ unlines
        $ map unwords
        $ convert
        $ fromEither
        $ parseTo 
        $ contents ++ "\n"
    hClose handle

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = case ioeGetFileName e of
        Just path -> putStrLn $ "File does not exist at: " ++ path
        Nothing   -> putStrLn "File does not exist at"
    | otherwise = ioError e

convert :: [[String]] -> [[String]]
convert = map $ map (toMD . toJian)

wyFile = endBy line eol
line = sepBy cell (char ' ')
cell = many (noneOf " \n")
eol = char '\n'

parseTo :: String -> Either ParseError [[String]]
parseTo = parse wyFile "(unknown)"

fromEither :: Either a b -> b
fromEither (Right x) = x
fromEither (Left  _) = error "Probably syntax error"
