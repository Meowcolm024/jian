module Jian where

import Data.List

data JianVal = Heading Int String
             | Body [String]
             | None
             deriving (Show)

heading :: String -> JianVal
heading x 
    | "書》" `isSuffixOf` x = Heading 1 (toHeading x)
    | "卷》" `isSuffixOf` x = Heading 2 (toHeading x)
    | "篇》" `isSuffixOf` x = Heading 3 (toHeading x)
    where 
        getTitle t = if "《" `isPrefixOf` t then tail t else t
        toHeading h = init $ init $ getTitle h
heading _ = None
