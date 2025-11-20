module Parser (
    Parser (..),
    Alternative (..),
    runParser,
    parseFailure,
    parseItem,
    parseIs,
    parseIsOneOf,
    parseChar,
    parseDigit,
    parseString,
    parseCount,
    parseInt,
    parseUnsignedInt,
    parseSpaces,
    parseLineReturn,
    parseEither,
    parseLine,
    parseSelect,
    parseMaybe,
)
where

import Control.Applicative
import Data.Char

-- A mini parser
newtype Parser a = Parser {parse :: String -> [(a, String)]}

-- Make the Parser an instance of Functor so we may map things
instance Functor Parser where
    -- (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \s -> [(f a, s') | (a, s') <- p s]

-- Make the Parser an instance of Applicative so we may lift it
instance Applicative Parser where
    -- a -> Parser a
    pure a = Parser $ \s -> [(a, s)]

    -- Parser (a -> b) -> Parser a -> Parser b
    (<*>) (Parser ab) (Parser b) = Parser $ \s -> [(f a, s'') | (f, s') <- ab s, (a, s'') <- b s']

-- Make the Parser an instance of Monad so that we may use the 'do' notation
instance Monad Parser where
    -- a -> Parser a
    -- pure a = Parser $ \s -> [(a, s)]
    -- Parser a -> (a -> Parser b) -> Parser b
    (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') (parse p s)

-- Make the Parser an instance of Alternative so that we may process choice
instance Alternative Parser where
    -- Parser a
    empty = parseFailure

    -- Parser a -> Parser a -> Parser a
    (<|>) p p' = Parser $ \s ->
        case parse p s of
            [] -> parse p' s
            r -> r

-- Runs the parser
runParser :: Parser a -> String -> a
runParser p s = case parse p s of
    [(r, _)] -> r
    _ -> error "Failed to parse string"

-- Failure (can be used to stop another parser)
parseFailure :: Parser a
parseFailure = Parser $ \_ -> []

-- Parses a single item
parseItem :: Parser Char
parseItem = Parser $ \s -> case s of
    [] -> []
    (c : s') -> [(c, s')]

-- Parses an item & executes a predicate
parseIs :: (Char -> Bool) -> Parser Char
parseIs p = Parser $ \s -> case s of
    [] -> []
    (c : s') ->
        if p c
            then [(c, s')]
            else []

-- Parses an item & compares it against a set of characters
parseIsOneOf :: [Char] -> Parser Char
parseIsOneOf cs = parseIs (flip elem cs)

-- Parses the single characters
parseChar :: Char -> Parser Char
parseChar c = parseIs (== c)

-- Parses a single digit
parseDigit :: Parser Char
parseDigit = parseIs (isDigit)

-- Parses the given string
parseString :: String -> Parser String
parseString [] = pure []
parseString (c : s) = do
    _ <- parseChar c
    _ <- parseString s
    return (c : s)

-- Parses a number of characters into a string
parseCount :: Int -> Parser String
parseCount 0 = pure []
parseCount n = do
    c <- parseItem
    cs <- parseCount (n - 1)
    return $ (c : cs)

-- Parses an int
parseInt :: Parser Int
parseInt = do
    s <- parseString "-" <|> return []
    ds <- some parseDigit
    return $ read (s ++ ds)

-- Parses an unsigned int
parseUnsignedInt :: Parser Int
parseUnsignedInt = do
    ds <- some parseDigit
    return $ read ds

-- Parses any space(s)
parseSpaces :: Parser String
parseSpaces = many (parseIsOneOf " \r\n")

-- Parse line return
parseLineReturn :: Parser String
parseLineReturn = (parseString "\r\n") <|> (parseString "\r") <|> (parseString "\n")

-- Runs the parser and tries to populate an either type, first populating the left and then the right if the first fails
parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither pa pb = fmap Left pa <|> fmap Right pb

-- Selects either using parser a or b depending on the state of the flag
parseSelect :: Bool -> Parser a -> Parser b -> Parser (Either a b)
parseSelect True pa _ = fmap Left pa
parseSelect False _ pb = fmap Right pb

-- Runs the parser and tries to execute, if it cannot, no failure is emitted
parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe pa = fmap Just pa <|> pure Nothing

-- Parses a single line using the specified parser
parseLine :: Parser a -> Parser a
parseLine p = do
    a <- p
    _ <- parseMaybe parseLineReturn
    pure a
