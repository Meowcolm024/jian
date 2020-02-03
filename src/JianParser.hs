module JianParser where

import           Text.Parsec.String
import           Text.ParserCombinators.Parsec
                                         hiding ( many
                                                , (<|>)
                                                )
import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , (<*)
                                                , (*>)
                                                , (<|>)
                                                , many
                                                , (<$)
                                                )
import           Control.Monad                  ( void
                                                , ap
                                                )
import           Data.Char                      ( isLetter
                                                , isDigit
                                                )
import           Hanzi
{-
data JianVal = Heading Int String
             | OrdList Int String
             | UoList String
             | Quote String
             | Raw String
             | Comment String
             | Image String String
             | Link String String
             | Code String
             deriving (Show)
-}
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

image :: Parser String
image = do
    string "【有圖者「"
    title <- many1 $ noneOf "」"
    string "」自「"
    url <- many1 $ noneOf "」"
    string "」來】"
    return ("![" ++ title ++ "](" ++ url ++ ")")

url :: Parser String
url = do
    string "【有扉者「"
    title <- many1 $ noneOf "」"
    string "」通「"
    url <- many1 $ noneOf "」"
    string "」也】"
    return $ "![" ++ title ++ "](" ++ url ++ ")"

ordlist :: Parser String
ordlist = do
    id <- many1 $ oneOf "零一二三四五六七八九十百千萬"
    char '、'
    txt <- many1 $ noneOf "\n"
    return $ show (shuziToInt id) ++ txt

unordlist :: Parser String
unordlist = do
    char '〇'
    txt <- many1 $ noneOf "\n"
    return $ "* " ++ txt

parseLine :: Parser String
parseLine = do
    void $ many (char ' ')
    first   <- letter <|> char ' '
    rest    <- many (letter <|> char ' ')
    newline <- optionMaybe $ char '\n'
    return $ case newline of
        Just _  -> first : rest ++ " "
        Nothing -> first : rest
