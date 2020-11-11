-- |
--
-- Desugar.DesugaredLang defines the Abstract Syntac Tree of the desugared While+ language.
-- 
-- A desugared program is defined as a sequence of statements.
-- > newtype DesugaredProgram = DesugaredProgram [Statement]
--
-- Statement is the ADT for While+ statements.
-- AExpr is the ADT for arithmetical expressions.
-- BExpr is the ADT for boolean expressions.
-- Variable names are represented as strings.
--

module Desugar.DesugaredLang (
    Name
  , DesugaredProgram (..)
  , Statement (..)
  , BExpr (..)
  , BoolUnOp (..)
  , BoolBinOp (..)
  , ArithRelation (..)
  , AExpr (..)
  , ArithBinOp (..)
) where

import qualified Lang as L

-- | variable and functions identifiers are represented as strings
type Name = L.Name

-- | a program is just a sequence of statements
newtype DesugaredProgram = DesugaredProgram [Statement]
  deriving (Show, Eq)

-- | ADT for statements
data Statement
  = Assignment Name AExpr                  -- ^ numeral variable definition
  | Skip                                   -- ^ no-op
  | Composition [Statement]                -- ^ sequential composition of multiple statements
  | Conditional BExpr Statement Statement  -- ^ if b then S1 else S2
  | While BExpr Statement                  -- ^ while loop implemented natively
  | Repeat' Statement BExpr                -- ^ repeat-until loop implemented natively
  deriving (Show, Eq)

-- | ADT for boolean expressions
data BExpr
  = BoolLit Bool                     -- ^ boolean literals
  | BoolUnOp BoolUnOp BExpr          -- ^ boolean unary operators
  | BoolBinOp BoolBinOp BExpr BExpr  -- ^ boolean binary operators
  | ArithRelation                    -- ^ arithmetic binary relations with boolean results
      ArithRelation AExpr AExpr  
  deriving (Show, Eq)

-- | ADT for boolean unary operators
data BoolUnOp
  = OpNeg  -- ^ negation
  deriving (Show, Eq)

-- | ADT for boolean binary operators
data BoolBinOp
  = OpAnd   -- ^ a && b
  deriving (Show, Eq)

-- | ADT for arithmetic binary relations with boolean results
data ArithRelation
  = OpEq    -- ^ a == b
  | OpLte   -- ^ a <= b
  deriving (Show, Eq)

-- | ADT for arithmetic expressions
data AExpr
  = NumLit Integer                     -- ^ integral numeral constant
  | AVar Name                          -- ^ integral numeral variable
  | ArithBinOp ArithBinOp AExpr AExpr  -- ^ arithmetic binary operators with numerical results
  deriving (Show, Eq)

-- | ADT for arithmetic binary operators
data ArithBinOp
  = OpSum   -- ^ x + y
  | OpMul   -- ^ x * y
  | OpSub   -- ^ x - y
  deriving (Show, Eq)
