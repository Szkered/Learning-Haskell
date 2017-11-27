module Main where
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Char
import Data.Ratio
import Data.Complex


main = do args <- getArgs
          putStrLn (readExpr $ args!!0)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | Character Char
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Bool Bool
             | Ratio Rational
             | Complex (Complex Double)

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = fromIntegral n

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\""
                  return $ case x of
                             '\\' -> x
                             '"'  -> x
                             'n'  -> '\n'
                             'r'  -> '\r'
                             't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
          char '#'
          (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= return . Number . read

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   d <- many1 digit
                   return . Number . read $ d

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              d <- many1 hexDigit
              return . Number . hex2dig $ d

parseOct :: Parser LispVal
parseOct = do try $ string "#x"
              d <- many1 octDigit
              return . Number . oct2dig $ d

parseBin :: Parser LispVal
parseBin = do try $ string "#x"
              d <- many1 (oneOf "10")
              return . Number . bin2dig $ d

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return . Float . float2dig $ (x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do numerator <- many1 digit
                char '/'
                denominator <- many1 digit
                return $ Ratio ((read numerator) % (read denominator))

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal1)
                  char '+'
                  y <- (try parseFloat <|> parseDecimal1)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0 where
        bin2dig' digint "" = digint
        bin2dig' digint (x:xs) = let old = 2 * digint + toInteger (digitToInt x)
                                 in bin2dig' old xs
float2dig x = fst $ readFloat x !! 0


-- my attempt...
parseChar :: Parser LispVal
parseChar = do char '#'
               char '\\'
               first <- letter <|> symbol
               rest <- many (letter)
               case first of
                 'a' -> return . Character $ rest!!0
                 'A' -> return . Character $ rest!!0
                 '(' -> return . Character $ '('
                 ' ' -> return . Character $ ' '
                 's' -> return . Character $ ' '
                 'n' -> return . Character $ '\n'

parseCharacter :: Parser LispVal
parseCharacter = do
               try $ string "#\\"
               val <- try (string "newline" <|> string "space")
                     <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
               return $ Character $ case val of
                                      "space"   -> ' '
                                      "newline" -> '\n'
                                      _         -> (val!!0)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat

readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value"
