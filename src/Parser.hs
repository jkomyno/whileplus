-- |
--
-- Parser defines a parser that given a source code string creates an AST for the While+ language.
-- The tokenization, lexemization and parsing steps are taken care of using the Parsec library.
-- Parsers are defined as a sequence of alternative subparsers.
-- For each arithmetic and boolean operation, we also defined the corresponding associativity and
-- precedence rules.

module Parser (parseProgram) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Grammar
import Data.Functor.Identity (Identity)
import Data.Bifunctor (first)
import Prelude hiding (repeat)

import Lang
import qualified LangException as LE

------------------------------------------------------
-- * Parsec style definition and lexer initializations
------------------------------------------------------

-- | Language token style definition. It defines:
-- 1. how comments are delimited
-- 2. the valid characters for defining identifiers
-- 3. the reserved keywords
-- 4. the reserved operators
style :: Tok.GenLanguageDef String () Identity
style = Grammar.emptyDef {
  -- describes the start of a block comment
    Tok.commentStart = "/*"

  -- describes the end of a block comment
  , Tok.commentEnd = "*/"

  -- describes the start of a line comment
  , Tok.commentLine = "//"

  -- parser for legal start characters of idenfiers
  , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"

  -- parser for legal tail characters of idenfiers
  , Tok.identLetter = digit <|> letter <|> oneOf "_"

  -- the language is case sensitive
  , Tok.caseSensitive = True

  -- list of reserved keywords
  , Tok.reservedNames = [
      "true"
    , "false"
    , "skip"
    , "if"
    , "then"
    , "else"
    , "while"
    , "do"
    , "repeat'"
    , "repeat"
    , "until"
    , "for"
    , "to"]

  -- list of reserved operators
  , Tok.reservedOpNames = [
      ","
    , "+"
    , "-"
    , "*"
    , "+="
    , "-="
    , "*="
    , ":="
    , "!"
    , "="
    , "!="
    , "<"
    , ">"
    , "<="
    , ">="
    , "&&"
    , "||"
    , "("
    , ")"]
  }

-- | Lexer generated from the language style
lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser style

-- | Parses the given string and checks that the given symbol name is not a prefix of a valid identifier
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | Parses the given string and checks that the given symbol name is not a prefix of a valid operator
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | First applies the given parser and then the whiteSpace parser.
-- It returns the value of the given parser.
lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

-- | Parses the given parser enclosed in parenthesis, returning
-- the result of the given parser
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Parses any white space.
-- White space consists of zero or more occurrences of a space,
-- a line comment or a block (multi line) comment
whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- | Parser for the semicolon (;) symbol
semi :: Parser String
semi = Tok.semi lexer

-- | Parser for a legal identifier
identifier :: Parser String
identifier = Tok.identifier lexer

-- | Parser for integer numbers
integer :: Parser Integer
integer = Tok.integer lexer

-- | Parser for boolean constants
bool :: Parser Bool
bool = (True <$ reserved "true")
    <|> (False <$ reserved "false")


------------------------------------------
-- * Top-level language parser definitions
------------------------------------------


-- | Parser for a While program
program :: Parser Program
program = Program <$> statements

-- | Parser for a sequence of statements.
-- Each statement must terminate with a semicolon symbol @';'@.
-- Optionally, the last statement can avoid doing so.
statements :: Parser [Statement]
statements = statement `sepEndBy1` semi

statement :: Parser Statement
statement = composition
        <|> skip
        <|> conditional
        <|> while
        <|> repeat'
        <|> repeat
        <|> for
        <|> try assignment
        <|> opAssignment

-- Parser for arithmetic expressions
aExpr :: Parser AExpr
aExpr = let
  -- arithmetic operators with precedence and associativity definitions
  ops = [
      -- multiplication is infix
      [Infix (reservedOp "*" >> return (ArithBinOp OpMul)) AssocLeft]

      -- sum is infix
    , [Infix (reservedOp "+" >> return (ArithBinOp OpSum)) AssocLeft]

    -- subtraction is infix
    , [Infix (reservedOp "-" >> return (ArithBinOp OpSub)) AssocLeft]]

  -- arithmetic expression parser
  term = parens aExpr
      <|> variable
      <|> numLiteral
  
  in buildExpressionParser ops term

-- Parser for boolean expressions
bExpr :: Parser BExpr
bExpr = let
  -- boolean operators with precedence and associativity definitions
  ops = [
    -- conjunction and disjunction are infix
    [
      Infix (reservedOp "&&" >> return (BoolBinOp OpAnd)) AssocLeft
    , Infix (reservedOp "||" >> return (BoolBinOp OpOr)) AssocLeft]

    -- negation is prefix
    , [Prefix (reservedOp "!" >> return (BoolUnOp OpNeg))]]

  -- boolean expression parser
  term = parens bExpr
      <|> boolLiteral
      <|> arithRelation

  in buildExpressionParser ops term

-- Parser for arithmetic binary relations with boolean results
arithRelation :: Parser BExpr
arithRelation = do
  a1 <- aExpr
  op <- arithRelationOp
  a2 <- aExpr
  return $ op a1 a2

arithRelationOp :: Parser (AExpr -> AExpr -> BExpr)
arithRelationOp = (ArithRelation OpEq <$ reservedOp "=")
               <|> (ArithRelation OpNeq <$ reservedOp "!=")
               <|> (ArithRelation OpLt <$ reservedOp "<")
               <|> (ArithRelation OpGt <$ reservedOp ">")
               <|> (ArithRelation OpLte <$ reservedOp "<=")
               <|> (ArithRelation OpGte <$ reservedOp ">=")

----------------------------------------------
-- Second-level language parser definitions --
----------------------------------------------

-- Parser for integer literals
numLiteral :: Parser AExpr
numLiteral = NumLit <$> integer

-- Parser for variables
variable :: Parser AExpr
variable = AVar <$> identifier

-- Parser for boolean literals
boolLiteral :: Parser BExpr
boolLiteral = BoolLit <$> bool

-- Parser for composition statement
composition :: Parser Statement
composition = do
  reservedOp "("
  ss <- statements
  reservedOp ")"
  case ss of
    [s] -> return s
    _   -> return $ Composition ss

-- Parser for variable assignment
assignment :: Parser Statement
assignment = do
  name <- identifier
  reservedOp ":="
  body <- aExpr
  return $ Assignment name body

-- Parser for the symbol of sugar assignment + arithmetic binary operator
arithBinOpAssignment :: Parser ArithBinOp
arithBinOpAssignment = (reservedOp "+=" >> return OpSum)
                   <|> (reservedOp "-=" >> return OpSub)
                   <|> (reservedOp "*=" >> return OpMul)

-- Parser for sugar assignment + arithmetic binary operator
opAssignment :: Parser Statement
opAssignment = do
  name <- identifier
  op   <- arithBinOpAssignment
  body <- aExpr
  return $ OpAssignment name op body

-- Parser for a skip expression
skip :: Parser Statement
skip = Skip <$ reserved "skip"

-- Parser for conditional if-then-else expressions
conditional :: Parser Statement
conditional = do
    reserved "if"
    b <- bExpr
    reserved "then"
    t <- statement
    reserved "else"
    f <- statement
    return $ Conditional b t f

-- Parser for while-loop
while :: Parser Statement
while = do
  reserved "while"
  b <- bExpr
  reserved "do"
  s <- statement
  return $ While b s

-- Parser for for loop (to be desugared)
for :: Parser Statement
for = do
  reserved "for"
  (Assignment i start) <- assignment
  reserved "to"
  end  <- aExpr
  reserved "do"
  s <- statement
  return $ For i start end s

-- Parser for repeat-until loop (implemented natively)
repeat' :: Parser Statement
repeat' = do
  reserved "repeat'"
  s <- statement
  reserved "until"
  b <- bExpr
  return $ Repeat' s b

-- Parser for repeat-until loop (to be desugared)
repeat :: Parser Statement
repeat = do
  reserved "repeat"
  s <- statement
  reserved "until"
  b <- bExpr
  return $ Repeat s b


-----------------------------------------------------------
-- * Utilities for parsing files or lines submitted by user
-----------------------------------------------------------

-- Wrapper for a Parser that allows leading whitespace and a
-- terminal end of file (eof)
contents :: Parser a -> ParsecT String () Identity a
contents p = whiteSpace *> lexeme p <* eof

-- used to parse a While+ program
parseProgram :: String -> Either LE.LangException Program
parseProgram = first (LE.ParseError . show) . parse (contents program) "<stdin>"
