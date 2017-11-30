module Main where
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char
import Data.Ratio
import Data.Complex
import Data.Array
import Data.List
-- import Control.Monad.Except
import Control.Monad.Error

main = getArgs >>= print . eval . readExpr . head

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

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
             | Vector (Array Int LispVal)

showVal :: LispVal -> String
showVal (Atom name)            = name
showVal (Character name)       = show name
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (List contents)        = "(" ++ unwordList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents)      = show contents
showVal (Float contents)       = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (Ratio a)              = show (numerator a) ++ "/" ++ show (denominator a)
showVal (Complex c)            = show (realPart c) ++ "+" ++ show (imagPart c) ++ "i"
showVal (Vector array)         = "#(" ++ unwords valS ++ ")" where
                                    valS = map showVal . elems $ array

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
         noMsg  = Default "An error has occurred"
         strMsg = Default

type ThrowsError = Either LispError

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

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = fromIntegral n

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0 where
        bin2dig' digint "" = digint
        bin2dig' digint (x:xs) = let old = 2 * digint + toInteger (digitToInt x)
                                 in bin2dig' old xs
float2dig x = fst $ readFloat x !! 0

parseCharacter :: Parser LispVal
parseCharacter = do
               try $ string "#\\"
               val <- try (string "newline" <|> string "space")
                     <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
               return $ Character $ case val of
                                      "space"   -> ' '
                                      "newline" -> '\n'
                                      _         -> (val !! 0)

parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces1

parseDottedList :: Parser LispVal
parseDottedList = do
                head <- parseExpr `endBy` spaces1
                tail <- char '.' >> spaces1 >> parseExpr
                return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
            char '\''
            x <- parseExpr
            return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
                 char '`'
                 x <- parseExpr
                 return $ List [Atom "quasiquote", x]

parseUnQuoted :: Parser LispVal
parseUnQuoted = do
              char ','
              x <- parseExpr
              return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
            arrayVals <- parseExpr `sepBy` spaces1
            return $ Vector (listArray (0, (length arrayVals - 1)) arrayVals)

parseList' :: Parser LispVal
parseList' = do char '(' >> spaces
                head <- parseExpr `sepEndBy` spaces1
                do char '.' >> spaces1
                   tail <- parseExpr
                   spaces >> char ')'
                   return $ DottedList head tail
                 <|> (spaces >> char ')' >> (return $ List head))

parseVector' :: Parser LispVal
parseVector' = do try $ string "#(" >> spaces
                  arrayVals <- parseExpr `sepEndBy` spaces1
                  char ')'
                  return $ Vector (listArray (0, (length arrayVals - 1)) arrayVals)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnQuoted
        -- <|> try ( do string "#("; x <- parseVector; char ')'; return x )
        <|> try parseVector'
        <|> parseList'
        -- <|> do { char '('; x <- try parseList <|> parseDottedList; char ')'; return x }
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat

readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> String $ "No match: " ++ show err
                   Right val -> val

-- eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?" , unaryOp symbolp),
              ("string?" , unaryOp stringp),
              ("number?" , unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?" , unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)]

numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)]
                       in if null parsed
                             then 0
                             else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp (Atom _)         = Bool True
symbolp _                = Bool False
numberp (Number _)       = Bool True
numberp _                = Bool False
stringp (String _)       = Bool True
stringp _                = Bool False
boolp   (Bool _)         = Bool True
boolp   _                = Bool False
listp   (List _)         = Bool True
listp   (DottedList _ _) = Bool True
listp   _                = Bool False

symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""
