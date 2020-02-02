module Jian where

import           Data.List
import           Control.Applicative
import           Hanzi

data JianVal = Heading Integer String
             | OrdList Integer String
             | Quote Bool
             | Raw String
             deriving (Show)

toHead :: String -> Maybe JianVal
toHead x | "書》" `isTitle` x = Just $ Heading 1 (toHeading x)
         | "卷》" `isTitle` x = Just $ Heading 2 (toHeading x)
         | "篇》" `isTitle` x = Just $ Heading 3 (toHeading x)
  where
    isTitle suf line = "《" `isPrefixOf` line && suf `isSuffixOf` line
    toHeading = init . tail
toHead _ = Nothing

toOrdList :: String -> Maybe JianVal
toOrdList x =
    let (nums, content) = break (== '、') x
    in  if all isShuzi nums
            then Just $ OrdList (shuziToInt nums) (tail content)
            else Nothing

toRaw :: String -> Maybe JianVal
toRaw x = Just $ Raw x

toQuote :: String -> Maybe JianVal
toQuote "「「" = Just $ Quote True
toQuote "」」" = Just $ Quote False
toQuote _    = Nothing

toJian :: String -> Maybe JianVal
toJian x = toHead x <|> toOrdList x <|> toQuote x <|> toRaw x

