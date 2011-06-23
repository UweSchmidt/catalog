module Language.Tcl.Commands.Expr
where

import Control.Monad.RWS

import Data.List                        ( intercalate )

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.CheckArgs
import Language.Tcl.Expr.Eval           ( evalTclExpr )

-- ------------------------------------------------------------

tclExpr :: TclCommand e s
tclExpr al@(_ : _)
    = evalTclExpr (intercalate " " $ map v2s al)

tclExpr _
    = tclWrongArgs "expr arg ?arg ...?"

-- ------------------------------------------------------------

tclIncr :: TclCommand e s
tclIncr [var]
    = tclIncr [var, value_1]
tclIncr [varName, incr]
    = do v1 <- get >>= lookupVar var
	       >>= checkIntegerValue
         v2 <- checkIntegerValue incr
         get >>= setVar var (mkI $ v1 + v2)
    where
      var = v2s varName

tclIncr _
    = tclWrongArgs "incr varName ?increment?"

-- ------------------------------------------------------------