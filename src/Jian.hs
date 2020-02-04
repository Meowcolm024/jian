module Jian
  ( toJian
  , toMD
  )
where

import           Data.List
import           Control.Applicative
import           Hanzi

data JianVal = Heading Int String
             | OrdList Int String
             | UoList String
             | Quote Bool
             | Raw String
             | Comment String
             | Image String String
             | Link String String
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

toUoList :: String -> Maybe JianVal
toUoList "〇" = Nothing
toUoList x   = if "〇" `isPrefixOf` x then Just (UoList (tail x)) else Nothing

toRaw :: String -> Maybe JianVal
toRaw x = Just $ Raw x

toQuote :: String -> Maybe JianVal
toQuote "「「" = Just $ Quote True
toQuote "」」" = Just $ Quote False
toQuote _    = Nothing

toComment :: String -> Maybe JianVal
toComment x = if "批：" `isPrefixOf` x then Just (Comment x) else Nothing

toImage :: String -> Maybe JianVal
toImage x = if "【有圖者" `isPrefixOf` x && "來】" `isSuffixOf` x
  then
    let name = tail $ (takeWhile (/= '」') . dropWhile (/= '「')) x
        url  = (tail . tail) $ (takeWhile (/= '」') . dropWhile (/= '自')) x
    in  Just (Image name url)
  else Nothing

toLink :: String -> Maybe JianVal
toLink x = if "【有扉者" `isPrefixOf` x && "也】" `isSuffixOf` x
  then
    let name = tail $ (takeWhile (/= '」') . dropWhile (/= '「')) x
        url  = (tail . tail) $ (takeWhile (/= '」') . dropWhile (/= '通')) x
    in  Just (Link name url)
  else Nothing

isBlank :: String -> Maybe JianVal
isBlank "" = Just $ Raw ""
isBlank _  = Nothing

toJian :: String -> Maybe JianVal
toJian x =
  isBlank x
    <|> toHead x
    <|> toOrdList x
    <|> toUoList x
    <|> toQuote x
    <|> toComment x
    <|> toImage x
    <|> toLink x
    <|> toRaw x

toMD :: Maybe JianVal -> String
toMD (Just (Raw x         )) = x
toMD (Just (Heading h x   )) = replicate h '#' ++ " " ++ x
toMD (Just (OrdList h x   )) = show h ++ ". " ++ x
toMD (Just (UoList  x     )) = "- " ++ x
toMD (Just (Quote   x     )) = if x then "<blockquote>" else "</blockquote>"
toMD (Just (Comment x     )) = "<!--" ++ x ++ "-->"
toMD (Just (Image name url)) = "![" ++ name ++ "](" ++ url ++ ")"
toMD (Just (Link  name url)) = "[" ++ name ++ "](" ++ url ++ ")"
toMD _                       = ""
