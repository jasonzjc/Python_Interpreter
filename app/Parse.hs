module Parse where

import Data.Char (isDigit, isLetter, isPrint)
import Data.Functor.Identity
import Lib
import Text.Parsec.Char (char, digit, oneOf, satisfy)
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.String hiding (Parser)
import Text.ParserCombinators.Parsec hiding (Parser)

--- Parser
--- ------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- for testing a parser directly
run :: Parser a -> String -> a
run p s =
  case parse p "<stdin>" s of
    Right x -> x
    Left x -> error $ show x

-- Lexicals

-- symbol
symbol :: String -> Parser String
symbol s = do
  string s
  spaces
  return s

-- positive integer and zero
int :: Parser Int
int = do
  digits <- many1 digit <?> "an integer"
  spaces
  return (read digits :: Int)

-- negative interger
negint :: Parser Int
negint = do
  symbol "-"
  digits <- many1 digit <?> "a negative integer"
  spaces
  return (read $ "-" <> digits :: Int)

-- double
double :: Parser Double
double = do
  whole <- many1 digit
  char '.'
  fract <- many1 digit
  return (read $ whole <> "." <> fract)

-- negative double
negdouble :: Parser Double
negdouble = do
  symbol "-"
  whole <- many1 digit
  char '.'
  fract <- many1 digit
  return (read $ "-" <> whole <> "." <> fract)

-- number :: Parser (Num a)
-- number = try double <|> negint <|> int

-- variable
-- refernece: https://jakewheat.github.io/intro_to_parsing/#_num
var :: Parser String
var = do
  fc <- firstChar
  rest <- many nonFirstChar
  spaces
  return (fc : rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

-- blank space
spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"

-- string
stringP :: Parser String
stringP = do
  symbol "\""
  --      s <- many1 letter <*> (spaces *> many1 digit) <?> "a string"
  s <- many characters <?> "a string"
  symbol "\""
  return s
  where
    characters = satisfy (\a -> isPrint a && a /= '\"')

-- paranthesis
parens :: Parser a -> Parser a
parens p = do
  symbol "("
  pp <- p
  symbol ")"
  return pp

-- Expressions

intExp :: Parser Exp
intExp = do
  i <- int
  return $ IntExp i

negintExp :: Parser Exp
negintExp = do
  i <- negint
  return $ IntExp i

doubleExp :: Parser Exp
doubleExp = do
  f <- double
  return $ DoubleExp f

negdoubleExp :: Parser Exp
negdoubleExp = do
  f <- negdouble
  return $ DoubleExp f

boolExp :: Parser Exp
boolExp =
  (symbol "True" >> return (BoolExp True))
    <|> (symbol "False" >> return (BoolExp False))

varExp :: Parser Exp
varExp = do
  v <- var
  return $ VarExp v

strExp :: Parser Exp
strExp = do
  s <- stringP
  return $ StrExp s

noneExp :: Parser Exp
noneExp = (symbol "None" >> return NoneExp)

monopExp :: (String -> Exp -> Exp) -> String -> Parser (Exp -> Exp)
monopExp ctor str = symbol str >> return (ctor str)

notExp :: Parser Exp
notExp = do
  try $ symbol "not"
  e1 <- expr
  return $ NotExp e1

opExp :: (String -> Exp -> Exp -> Exp) -> String -> Parser (Exp -> Exp -> Exp)
opExp ctor str = symbol str >> return (ctor str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp =
  let mulOpExp = opExp NumOpExp
   in mulOpExp "*" <|> mulOpExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp =
  let addOpExp = opExp NumOpExp
   in addOpExp "+" <|> addOpExp "-"

andOp :: Parser (Exp -> Exp -> Exp)
andOp = opExp BoolOpExp "and"

orOp :: Parser (Exp -> Exp -> Exp)
orOp = opExp BoolOpExp "or"

compOp :: Parser (Exp -> Exp -> Exp)
compOp =
  let compOpExp s = symbol s >> return (CompOpExp s)
   in try (compOpExp "<=")
        <|> try (compOpExp ">=")
        <|> compOpExp "/="
        <|> compOpExp "=="
        <|> compOpExp "<"
        <|> compOpExp ">"

ifExp :: Parser Exp
ifExp = do
  try $ symbol "if"
  e1 <- expr
  symbol ":"
  e2 <- expr
  symbol "else:"
  e3 <- expr
  symbol "fi"
  return $ IfExp e1 e2 e3

funExp :: Parser Exp
funExp = do
  try $ symbol "fn"
  symbol "["
  params <- var `sepBy` (symbol ",")
  symbol "]"
  body <- expr
  symbol "end"
  return $ FunExp params body

letExp :: Parser Exp
letExp = do
  try $ symbol "let"
  symbol "["
  params <-
    ( do
        v <- var
        symbol ":="
        e <- expr
        return (v, e)
      )
      `sepBy` (symbol ";")
  symbol "]"
  body <- expr
  symbol "end"
  return $ LetExp params body

appExp :: Parser Exp
appExp = do
  try $ symbol "apply"
  efn <- expr
  symbol "("
  exps <- expr `sepBy` (symbol ",")
  symbol ")"
  return $ AppExp efn exps

-- absExp :: Parser Exp
-- absExp = do try $ symbol "abs"
--             symbol "("
--             e1 <- expr
--             symbol ")"
--             return $ AbsExp e1

expr :: Parser Exp
expr =
  let disj = conj `chainl1` andOp
      --   notj = conj `chainl1` notOp
      conj = arith `chainl1` compOp
      arith = term `chainl1` addOp
      term = factor `chainl1` mulOp
      factor = atom
   in disj `chainl1` orOp

atom :: Parser Exp
atom =
  try doubleExp
    <|> try negdoubleExp
    <|> intExp
    <|> negintExp
    <|> notExp
    <|> strExp
    <|> noneExp
    <|> funExp
    <|> ifExp
    <|> letExp
    <|> try boolExp
    <|> appExp
    <|> varExp
    --    <|> absExp
    <|> parens expr

-- Statements

quitStmt :: Parser Stmt
quitStmt = do
  try $ symbol "exit()"
  return QuitStmt

printStmt :: Parser Stmt
printStmt = do
  try $ symbol "print"
  symbol "("
  e <- expr
  symbol ")"
  return $ PrintStmt e

setStmt :: Parser Stmt
setStmt = do
  v <- var
  symbol "="
  e <- expr
  return $ SetStmt v e

ifStmt :: Parser Stmt
ifStmt = do
  try $ symbol "if"
  e1 <- expr
  symbol ":"
  s2 <- stmt
  symbol "else:"
  s3 <- stmt
  symbol "fi"
  return $ IfStmt e1 s2 s3

procStmt :: Parser Stmt
procStmt = do
  try $ symbol "procedure"
  name <- var
  symbol "("
  params <- var `sepBy` (symbol ",")
  symbol ")"
  body <- stmt
  symbol "endproc"
  return $ ProcedureStmt name params body

callStmt :: Parser Stmt
callStmt = do
  try $ symbol "call"
  name <- var
  symbol "("
  args <- expr `sepBy` (symbol ",")
  symbol ")"
  symbol ";"
  return $ CallStmt name args

seqStmt :: Parser Stmt
seqStmt = do
  try $ symbol "do"
  stmts <- many1 stmt
  symbol "od"
  symbol ";"
  return $ SeqStmt stmts

expStmt :: Parser Stmt
expStmt = do
  symbol "("
  e1 <- expr
  symbol ")"
  return $ ExpStmt e1

absStmt :: Parser Stmt
absStmt = do
  try $ symbol "abs"
  symbol "("
  e1 <- expr
  symbol ")"
  return $ AbsStmt e1

boolStmt :: Parser Stmt
boolStmt = do
  try $ symbol "bool"
  symbol "("
  e1 <- expr
  symbol ")"
  return $ BoolStmt e1

charStmt :: Parser Stmt
charStmt = do
  try $ symbol "chr"
  symbol "("
  e1 <- expr
  symbol ")"
  return $ CharStmt e1

evalStmt :: Parser Stmt
evalStmt = do
  try $ symbol "eval"
  symbol "("
  e1 <- expr
  symbol ")"
  return $ EvalStmt e1

roundStmt :: Parser Stmt
roundStmt = do
  try $ symbol "round"
  symbol "("
  e1 <- expr
  symbol ","
  e2 <- expr
  symbol ")"
  return $ RoundStmt e1 e2

stmt :: Parser Stmt
stmt =
  quitStmt
    <|> printStmt
    <|> ifStmt
    <|> procStmt
    <|> callStmt
    <|> seqStmt
    <|> absStmt
    <|> boolStmt
    <|> charStmt
    <|> expStmt
    <|> evalStmt
    <|> roundStmt
    <|> try setStmt

--    <|> try expStmt