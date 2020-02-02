module Lib
    ( someFunc
    )
where

import           Text.ParserCombinators.Parsec
import           System.IO
import           System.IO.Error
import           Control.Exception
import           System.Environment
import           Jian

someFunc :: IO ()
someFunc = print $ toJian "十一、滑稽者"
    -- getFile `catch` handler

getFile :: IO ()
getFile = do
    (name : _) <- getArgs
    handle     <- openFile name ReadMode
    hSetEncoding handle utf8
    contents <- hGetContents handle
    writeFile (takeWhile (/= '.') name ++ ".md") "something"
    hClose handle

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = case ioeGetFileName e of
        Just path -> putStrLn $ "File does not exist at: " ++ path
        Nothing   -> putStrLn "File does not exist at"
    | otherwise = ioError e
