module JianParser
    ( jianToMD
    )
where

import           Text.Parsec.String
import           Text.ParserCombinators.Parsec
import           Control.Monad                  ( void )
import           Hanzi

data JianVal = Heading Int String
             | Body [JianVal]
             | Line String
             | InLine String
             | CodeBlock String String
             | Image String String
             | Link String String
             | Comment String
             | OrdList Int String
             | UnoList String
             | Quote Bool
             | End
            deriving (Show)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

heading :: Parser JianVal
heading = do
    lv   <- optionMaybe $ many1 space
    rest <- many1 $ choice [letter, char '·']
    choice [eof, void (char '\n')]
    return $ case lv of
        Just lv' -> Heading (length lv' `div` 2 + 1) rest
        Nothing  -> Heading 1 rest

line :: Parser JianVal
line = do
    txt  <- many1 $ noneOf "。？！：\n"
    rest <- oneOf "。？！：\n"
    return $ Line $ txt ++ [rest]

body :: Parser JianVal
body = do
    txt <- many1 $ choice [try image, try url, try inline, line]
    choice [eof, void (char '\n')]
    return $ Body txt

inline :: Parser JianVal
inline = do
    string "〔"
    txt <- many1 $ noneOf "〕"
    string "〕"
    void $ optionMaybe $ choice [eof, void (oneOf " \n")]
    return $ InLine txt

codeBlock :: Parser JianVal
codeBlock = do
    string "〔〔書以："
    lang <- many1 $ noneOf "\n"
    code <- many1 $ noneOf "〕"
    string "〕〕"
    void $ choice [eof, void (oneOf " \n")]
    return $ CodeBlock lang code

image :: Parser JianVal
image = do
    string "【有圖者「"
    title <- many1 $ noneOf "」"
    string "」自「"
    url <- many1 $ noneOf "」"
    string "」來】"
    void $ optionMaybe $ choice [eof, void (oneOf " \n")]
    return $ Image title url

url :: Parser JianVal
url = do
    string "【有扉者「"
    title <- many1 $ noneOf "」"
    string "」通「"
    url <- many1 $ noneOf "」"
    string "」也】"
    choice [eof, void (oneOf " \n")]
    return $ Link title url

ordlist :: Parser JianVal
ordlist = do
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
    choice [string "批：", string "疏："]
    txt <- many1 $ noneOf "\n"
    return $ Comment txt

end :: Parser JianVal
end = do
    choice [string "【列終】", string "【空】", string "【終了】", many1 $ oneOf " \n\t"]
    choice [eof, void (char '\n')]
    return End

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
element = many1 $ choice
    [ try heading
    , try comment
    , try unordlist
    , try ordlist
    , try quote
    , try end
    , try codeBlock
    , body
    ]

render :: String -> [JianVal]
render x =
    let out = regularParse element x
    in  case out of
            Right good -> good
            _          -> []

renderBody :: JianVal -> String
renderBody x = case x of
    Line t         -> t
    Image name url -> "![" ++ name ++ "](" ++ url ++ ")\n"
    Link  name url -> "[" ++ name ++ "](" ++ url ++ ")\n"
    InLine t       -> "`" ++ t ++ "`"
    _              -> ""


toMdLine :: JianVal -> String
toMdLine x = case x of
    Heading h t   -> replicate h '#' ++ " " ++ t ++ "\n"
    Body t        -> concatMap renderBody t ++ "\n"
    OrdList h t   -> show h ++ ". " ++ t
    UnoList t     -> "- " ++ t
    Comment t     -> "<!--" ++ t ++ "-->"
    Quote   t     -> if t then "<blockquote>" else "</blockquote>\n"
    End           -> ""
    CodeBlock l c -> "``` " ++ l ++ c ++ "```\n"

jianToMD :: String -> String
jianToMD x = unlines $ map toMdLine (render x)
