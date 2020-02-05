module JianParser
    ( jianToMD
    )
where

import           Text.Parsec.String
import           Text.Parsec                    ( Parsec )
import           Text.ParserCombinators.Parsec
                                         hiding ( many
                                                , (<|>)
                                                )
import           Control.Applicative            ( (<|>)
                                                , many
                                                )
import           Control.Monad                  ( void
                                                , guard
                                                )
import           Data.List                      ( intercalate )
import           Hanzi

{-
I just can't make it work ==
-}

data JianVal = Heading Int String
             | Body String
             | Image String String
             | Link String String
             | Comment String
             | OrdList Int String
             | UnoList String
             | Quote Bool
             | EndOfList
            deriving (Show)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

heading :: Parser JianVal
heading = do
    lv   <- optionMaybe $ many1 space
    rest <- many1 letter
    choice [eof, void (char '\n')]
    return $ case lv of
        Just lv' -> Heading (length lv' `div` 2 + 1) rest
        Nothing  -> Heading 1 rest

line :: Parser JianVal
line = do
    txt <- many1 $ noneOf "\n"
    choice [eof, void (char '\n')]
    return $ Body txt

image :: Parser JianVal
image = do
    --void $ optionMaybe $ many1 anyToken
    string "【有圖者「"
    title <- many1 $ noneOf "」"
    string "」自「"
    url <- many1 $ noneOf "」"
    string "」來】"
    choice [eof, void (oneOf " \n")]
    return $ Image title url

url :: Parser JianVal
url = do
    --void $ optionMaybe $ many1 anyToken
    string "【有扉者「"
    title <- many1 $ noneOf "」"
    string "」通「"
    url <- many1 $ noneOf "」"
    EndOfList      -> ""
    id <- many1 $ satisfy isShuzi
    char '、'
    txt <- many1 $ noneOf "\n"
    choice [eof, void (char '\n')]
    return $ OrdList (shuziToInt id) txt

unordlist :: Parser JianVal
unordlist = do
    char '〇'
    txt <- many1 $ noneOf "\n"
    choice [eof, void (char '\n')]
    return $ UnoList txt

comment :: Parser JianVal
comment = do
    void $ many (char ' ')
    string "批："
    txt <- many1 $ noneOf "\n"
    return $ Comment txt

endoflist :: Parser JianVal
endoflist = do
    string "【列終】"
    choice [eof, void (char '\n')]
    return EndOfList

quote :: Parser JianVal
quote = choice [try quote1, try quote2]
  where
    quote1 :: Parser JianVal
    quote1 = do
        string "「「"
        choice [eof, void (char '\n')]
        return $ Quote True
    quote2 :: Parser JianVal
    quote2 = do
        string "」」"
        choice [eof, void (char '\n')]
        return $ Quote False

element :: Parser [JianVal]
element = do
    x <- many1 $ choice
        [ try heading
        , try comment
        , try image
        , try url
        , try unordlist
        , try ordlist
        , try quote
        , try endoflist
        , line
        ]
    return x

render :: String -> [JianVal]
render x =
    let out = regularParse element x
    in  case out of
            Right good -> good
            _          -> []

toMdLine :: JianVal -> String
toMdLine x = case x of
    Heading h t    -> "\n" ++ replicate h '#' ++ " " ++ t ++ "\n"
    Body t         -> t ++ "\n"
    OrdList h t    -> show h ++ ". " ++ t
    UnoList t      -> "- " ++ t
    Image name url -> "![" ++ name ++ "](" ++ url ++ ")"
    Link  name url -> "[" ++ name ++ "](" ++ url ++ ")"
    Comment t      -> "<!--" ++ t ++ "-->"
    Quote   t      -> if t then "<blockquote>" else "</blockquote>"
    EndOfList      -> ""

jianToMD :: String -> String
jianToMD x = unlines $ map toMdLine (render x)
