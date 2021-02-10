{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool

escapedChars :: Parser Char
escapedChars = do
    char '\\' --one backslash
    x <- oneOf "\\\"nrt" --backslash, quote, n, r, or t
    return $ case x of
        '\\' -> x
        '"'  -> x
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x
parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return (Atom atom)

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = Number . read <$> many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
    try $ string "#d"
    x <- many1 digit
    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "10")
    return $ Number (bin2dig x)
oct2dig x = fst $ head (readOct x)
hex2dig x = fst $ head (readHex x)
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                            bin2dig' old xs
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseBool
         <|> parseQuoted
         <|> parseList

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val

parseList :: Parser LispVal
parseList = between beg end parseList1
            where
                beg = char '(' >> skipMany space
                end = skipMany space >> char ')'
parseList1 :: Parser LispVal
parseList1 = do
    list <- sepEndBy parseExpr spaces
    maybeDatum <- optionMaybe (char '.' >> spaces >> parseExpr)
    return $ case maybeDatum of
        Nothing -> List list
        Just datum  -> DottedList list datum
-- parseList :: Parser LispVal
-- parseList = do
--     char '(' >> spaces
--     head <- parseExpr `sepEndBy` spaces1
--     do
--         char '.' >> spaces1
--         tail <- parseExpr
--         spaces >> char ')'
--         return $ DottedList head tail
--     <|> (spaces >> char ')' >> return (List head))

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
instance Show LispVal where show = showVal


main :: IO ()
main = do
            (expr:_) <- getArgs
            putStrLn (readExpr expr)

