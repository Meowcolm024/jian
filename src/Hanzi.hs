module Hanzi where

import           Data.List
import qualified Data.Map                      as Map

shuzi :: Map.Map Char Int
shuzi = Map.fromList $ zip "零一二三四五六七八九十" [0 .. 10]

wei :: Map.Map Char Int
wei = Map.fromList $ zip "十百千萬" [10, 100, 1000, 10000]

shuziToInt :: String -> Int
shuziToInt x = sum $ map groupDigit $ sepBy x []  where
    groupDigit :: String -> Int
    groupDigit [x] = fromJust (Map.lookup x shuzi)
    groupDigit [x, y] =
        fromJust (Map.lookup x shuzi) * fromJust (Map.lookup y wei)
    groupDigit _ = 0
    fromJust :: Maybe Int -> Int
    fromJust (Just i) = i
    fromJust Nothing  = 0
    sepBy :: String -> [String] -> [String]
    sepBy []  acc = acc
    sepBy [x] acc = acc ++ [[x]]
    sepBy org acc =
        let (left, s : right) = break (`elem` "零十百千萬") org
        in  sepBy right (acc ++ [left ++ [s]])

isShuzi :: Char -> Bool
isShuzi x = x `elem` "零一二三四五六七八九十百千萬"
