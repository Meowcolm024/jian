module Hanzi where

import           Data.List
import qualified Data.Map                      as Map

shuzi :: Map.Map Char Integer
shuzi = Map.fromList $ zip "零一二三四五六七八九十" [0 .. 10]

wei :: Map.Map Char Integer
wei = Map.fromList $ zip "十百千萬億" [10, 100, 1000, 10000, 100000]

shuziToInt :: String -> Integer
shuziToInt x = sum $ map groupDigit $ sepBy x [] where
    groupDigit :: String -> Integer
    groupDigit [x]    = fromJust (Map.lookup x shuzi)
    groupDigit [x, y] = fromJust (Map.lookup x shuzi) * fromJust (Map.lookup y wei)
    groupDigit _      = 0
    fromJust :: Maybe Integer -> Integer
    fromJust (Just i) = i
    fromJust Nothing  = 0
    sepBy :: String -> [String] -> [String]
    sepBy [] acc = acc
    sepBy [x] acc = acc ++ [[x]]
    sepBy org acc =
        let (left, s:right) = break (`elem` "十百千萬億") org
        in  sepBy right (acc ++ [left++[s]])

isShuzi :: Char -> Bool
isShuzi x = x `elem` "零一二三四五六七八九十百千萬億"
