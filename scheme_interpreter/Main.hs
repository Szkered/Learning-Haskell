{-# LANGUAGE ExistentialQuantification #-}

module Main where
import           Control.Monad
import           Data.Array
import           Data.Char
import           Data.Complex
import           Data.List
import           Data.Ratio
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec
import           Control.Monad.Error
-- import           Control.Monad.Loops

-- main = getArgs >>= print . eval . readExpr . head
main' = do
      args <- getArgs
      evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
      putStrLn $ extractValue $ trapError evaled

main = getArgs >>= rep >>= putStrLn . extractValue . trapError where
     re args = readExpr (head args) >>= eval
     rep     = return . liftM show . re

trapError action = catchError action (return . show)

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
             | Vector (Array Int LispVal) deriving (Eq)

showVal :: LispVal -> String
showVal (Atom name)            = name
showVal (Character name)       = show name
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents)      = show contents
showVal (Float contents)       = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (Ratio a)              = show (numerator a) ++ "/" ++ show (denominator a)
showVal (Complex c)            = show (realPart c) ++ "+" ++ show (imagPart c) ++ "i"
showVal (Vector array)         = "#(" ++ unwords valS ++ ")" where
                                    valS = map showVal . elems $ array

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
         noMsg  = Default "An error has occurred"
         strMsg = Default

type ThrowsError = Either LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> throwError $ Parser err
                   Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)                       = return val
eval val@(Number _)                       = return val
eval val@(Bool _)                         = return val
eval (List [Atom "quote", val])           = return val
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of
          Bool False -> eval alt
          Bool True  -> eval conseq
          _          -> throwError $ TypeMismatch "Bool" pred
eval form@(List (Atom "cond" : cs))       = if null cs
                                                then throwError $ BadSpecialForm "No clause in cond: " form
                                                else mapM evalClause cs >>= reduceClauses
eval form@(List (Atom "case" : key : cs)) = if null cs
                                                then throwError $ BadSpecialForm "No clause in cond: " form
                                                else eval key >>= caseExp cs
eval (List (Atom func : args))            = mapM eval args >>= apply func
eval badForm                              = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- The unspecified form is not implemented yet
reduceClauses :: [LispVal] -> ThrowsError LispVal
reduceClauses clauses = return $ last $ filter notFalse clauses
  where notFalse x = case boolp x of
                      Bool False -> True
                      Bool True  -> let (Bool result) = x in result

evalClause (List [Atom "else", sexp]) = eval sexp
evalClause (List [test, sexp])        = evalTest test sexp
evalClause (List [test])              = evalTest test $ Bool True
evalClause otherwise                  = throwError $ TypeMismatch "List of maximum 2 elements" otherwise

evalTest test sexp = eval test >>= unpackBool >>= (\b -> if b then eval sexp
                                                              else return $ Bool False)

evalTest' test sexp = do result <- eval test
                         case result of
                           Bool False -> return $ Bool False
                           Bool True  -> eval sexp
                           _          -> throwError $ TypeMismatch "Bool" test

caseExp' cs keyVal = case head cs of
                       List (Atom "else" : sexps) -> evalSexps sexps
                       List (List datum : sexps)  -> do
                        result <- mapM (\x -> eqv [keyVal, x]) datum
                        if Bool True `elem` result
                           then evalSexps sexps
                           else eval $ List (Atom "case" : List [Atom "quote", keyVal] : tail cs)
                       _                          -> throwError $ BadSpecialForm "Bad form: " (List cs)


-- caseExp :: [LispVal] -> LispVal -> ThrowsError LispVal
caseExp ((List (Atom "else" : sexps)) : []) keyVal = evalSexps sexps
caseExp ((List (List datum : sexps)) : []) keyVal  = matchCase datum sexps keyVal (return $ Atom "unspecified")
caseExp ((List (List datum : sexps)) : cs) keyVal  = matchCase datum sexps keyVal (caseExp cs keyVal)


matchCase :: [LispVal] -> [LispVal] -> LispVal -> ThrowsError LispVal -> ThrowsError LispVal
matchCase datum sexps keyVal f = do result <- mapM (\x -> eqv [keyVal, x]) datum
                                    if Bool True `elem` result
                                       then evalSexps sexps
                                       else f

evalSexps :: [LispVal] -> ThrowsError LispVal
evalSexps sexps = mapM eval sexps >>= return . last

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+"              , numericBinop (+)),
              ("-"              , numericBinop (-)),
              ("*"              , numericBinop (*)),
              ("/"              , numericBinop div),
              ("mod"            , numericBinop mod),
              ("quotient"       , numericBinop quot),
              ("remainder"      , numericBinop rem),
              ("symbol?"        , unaryOp symbolp),
              ("string?"        , unaryOp stringp),
              ("number?"        , unaryOp numberp),
              ("bool?"          , unaryOp boolp),
              ("list?"          , unaryOp listp),
              ("symbol->string" , unaryOp symbol2string),
              ("string->symbol" , unaryOp string2symbol),
              ("="              , numBoolBinop (==)),
              ("<"              , numBoolBinop (<)),
              (">"              , numBoolBinop (>)),
              ("/="             , numBoolBinop (/=)),
              (">="             , numBoolBinop (>=)),
              ("<="             , numBoolBinop (<=)),
              ("&&"             , boolBoolBinop (&&)),
              ("||"             , boolBoolBinop (||)),
              ("string=?"       , strBoolBinop (==)),
              ("string<?"       , strBoolBinop (<)),
              ("string>?"       , strBoolBinop (>)),
              ("string<=?"      , strBoolBinop (<=)),
              ("string>=?"      , strBoolBinop (>=)),
              ("car"            , car),
              ("cdr"            , cdr),
              ("cons"           , cons),
              ("eq?"            , eqv),
              ("eqv?"           , eqv),
              ("equal?"         , equal)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f []  = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []             = throwError $ NumArgs 2 []
numericBinop op singleVal@ [_] = throwError $ NumArgs 2 singleVal
numericBinop op params         = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left  <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)]
                       in if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

symbolp (Atom _) = Bool True
symbolp _        = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _) = Bool True
boolp   _        = Bool False
listp   (List _)         = Bool True
listp   (DottedList _ _) = Bool True
listp   _                = Bool False

symbol2string (Atom s) = String s
symbol2string _        = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]             = return $ List [x]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err         -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a . Eq a => Unpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (Unpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
             `catchError` (const $ return False)

-- need this return because after catchError we have the type Either LispError Bool
-- but we need ThrowsError Bool, which is a ThrowsError monad
-- a ThrowsError monad is different from a Either monad as the first argument type
-- (error type) is fixed

equal :: [LispVal] -> ThrowsError LispVal
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                               (all eqvPair $ zip arg1 arg2)
       where eqvPair (x1, x2) = case equal [x1, x2] of
                                  Left err         -> False
                                  Right (Bool val) -> val
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [Unpacker unpackNum, Unpacker unpackStr, Unpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


-- cond :: [LispVal] -> ThrowsError LispVal
-- cond xs = case odd . length $ xs of
--             True -> throwError $ Default "cond takes even number of condition"
--             False -> mapM evalClause $ splinter2 xs where
--                   evalClause (test, sexp) =

