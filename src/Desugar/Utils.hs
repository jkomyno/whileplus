module Desugar.Utils (negate) where

import Prelude hiding (negate)

import Desugar.DesugaredLang

negate :: BExpr -> BExpr
negate = BoolUnOp OpNeg
