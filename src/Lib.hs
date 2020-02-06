module Lib
    ( entryPoint
    )
where

import           System.IO
import           System.IO.Error
import           Control.Exception
import           System.Environment
import           JianParser

entryPoint :: IO ()
entryPoint = getFile `catch` handler

getFile :: IO ()
getFile = do
    (name : _) <- getArgs
    handle     <- openFile name ReadMode
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
