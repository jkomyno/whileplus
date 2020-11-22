-- |
--
-- Lang defines the Abstract Syntac Tree of the While+ language.
-- Some of the ADT constructors defined in Lang represent syntactic sugar,
-- so they will be rewritten in terms of other constructors before being
-- evaluated.
-- 
-- A program is defined as a sequence of statements.
-- > newtype Program = Program [Statement]
--
-- Statement is the ADT for While+ statements.
-- AExpr is the ADT for arithmetical expressions.
-- BExpr is the ADT for boolean expressions.
-- Variable names are represented as strings.
--

module Lang (
    Name
  , Program (..)
  , Statement (..)
  , BExpr (..)
  , BoolUnOp (..)
  , BoolBinOp (..)
  , ArithRelation (..)
  , AExpr (..)
  , ArithBinOp (..)
) where

-- | Variable and functions identifiers are represented as strings
type Name = String

-- | a program is just a sequence of statements
newtype Program = Program [Statement]
  deriving (Show, Eq)

-- | ADT for statements
data Statement
  = Assignment Name AExpr                  -- ^ numeral variable definition
  | Skip                                   -- ^ no-op
  | Composition [Statement]                -- ^ sequential composition of multiple statements
  | Conditional BExpr Statement Statement  -- ^ if b then S1 else S2
  | While BExpr Statement                  -- ^ while loop implemented natively
  | Repeat' Statement BExpr                -- ^ repeat-until loop implemented natively
  -- syntactic sugar starts
  | Repeat Statement BExpr                 -- ^ repeat-until based on while
  | For Name AExpr AExpr Statement         -- ^ for loop based on while
  | OpAssignment Name ArithBinOp AExpr     -- ^ assignment and arithmetic binary operation (+=, -=, *=)
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
  -- syntactic sugar starts
  | OpOr    -- ^ a || b
  deriving (Show, Eq)

-- | ADT for arithmetic binary relations with boolean results
data ArithRelation
  = OpEq    -- ^ a == b
  | OpLte   -- ^ a <= b
  -- syntactic sugar starts
  | OpNeq   -- ^ a != b
  | OpGte   -- ^ a >= b
  | OpGt    -- ^ a > b
  | OpLt    -- ^ a < b
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
