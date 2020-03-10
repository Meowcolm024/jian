module Cli
    ( cli
    )
where

import           System.IO
import           System.IO.Error
import           Control.Exception
import           System.Environment
import           JianParser

cli :: IO ()
cli = getArgs >>= processArg

processArg :: [String] -> IO ()
processArg a = if null a then help else entry $ head a

help :: IO ()
help = do
    putStrLn "Usage"
    putStrLn "  stack run [file]"

entry :: String -> IO ()
entry n = parseFile n `catch` handler

parseFile :: String -> IO ()
parseFile name = do
    handle <- openFile name ReadMode
    hSetEncoding handle utf8
    contents <- hGetContents handle
    writeFile (takeWhile (/= '.') name ++ ".md") $ jianToMD contents
    hClose handle

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = case ioeGetFileName e of
        Just path -> putStrLn $ "File does not exist at: " ++ path
        Nothing   -> putStrLn "File does not exist :("
    | otherwise = ioError e
