module Language.Tcl.Commands
where

import Control.Monad.Error
import Control.Monad.RWS

import Data.List                        ( intercalate )

import Language.Common.Eval

import Language.Tcl.Core
import Language.Tcl.CheckArgs
import Language.Tcl.Expr.Eval           ( evalTclExpr )

import System.IO

-- ------------------------------------------------------------

buildInTclCommands :: [(String, TclCommand e s)]
buildInTclCommands
    = [ ("append",      tclAppend)
      , ("break",       tclBreak)
      , ("continue",    tclContinue)
      , ("error",       tclError)
      , ("expr",        tclExpr)
      , ("foreach",     tclForeach)
      , ("if",          tclIf)
      , ("incr",        tclIncr)
      , ("puts", 	tclPuts)
      , ("return",      tclReturn)
      , ("set",		tclSet)
      ]

-- ------------------------------------------------------------

tclAppend :: TclCommand e s
tclAppend (varName : values)
    = do val <- (get >>= lookupVar varName)
                `mplus`
                return ""
         get >>= setVar varName (concat $ val : values)

tclAppend _
    = tclWrongArgs "append varName ?value value value ...?"

-- ------------------------------------------------------------

tclBreak :: TclCommand e s
tclBreak []
    = throwError $ tclBreakExc

tclBreak _
    = tclWrongArgs "break"

-- ------------------------------------------------------------

tclContinue :: TclCommand e s
tclContinue []
    = throwError $ tclContinueExc

tclContinue _
    = tclWrongArgs "continue"

-- ------------------------------------------------------------

tclError :: TclCommand e s
tclError [msg]
    = tclThrowError msg

tclError _
    = tclWrongArgs "error message"

-- ------------------------------------------------------------

tclExpr :: TclCommand e s
tclExpr al@(_ : _)
    = evalTclExpr (intercalate " " al) >>= return . show

tclExpr _
    = tclWrongArgs "expr arg ?arg ...?"

-- ------------------------------------------------------------

tclForeach :: TclCommand e s
tclForeach [var, list, body]
    = do xs <- evalTclList list
         cs <- parseTclProg body
         _  <- runForeach xs cs
         return ""
    where
      runForeach xs cs
          = tclCatchBreakExc $
              mapM_ oneStep xs
              >> return ""
          where
            oneStep val
                = ( get >>= setVar var val )
                  >> 
                  ( tclCatchContinueExc $
                      evalTclProg cs )

tclForeach _
    = tclWrongArgs "foreach varname list body"

-- ------------------------------------------------------------

tclIf :: TclCommand e s
tclIf (c : "then" : tp : rest)
    = tclIf' c tp rest
tclIf (c : tp : rest)
    = tclIf' c tp rest
tclIf _
    = tclWrongArgs "if expr1 ?then? body1 elseif expr2 ?then? body2 elseif ... ?else? ?bodyN?"

tclIf' :: String -> String -> TclCommand e s
tclIf' cond thn els
    = do b <- evalTclExpr cond >>= checkBooleanValue
         if b
            then interpreteTcl thn
            else tclElse       els

tclElse :: TclCommand e s
tclElse ("elseif" : ifcmd)
    = tclIf ifcmd
tclElse ("else" : els)
    = tclElse' els
tclElse els
    = tclElse' els

tclElse' :: TclCommand e s
tclElse' []
    = return ""
tclElse' [els]
    = interpreteTcl els
tclElse' _
    = tclIf []

-- ------------------------------------------------------------

tclIncr :: TclCommand e s
tclIncr [var]
    = tclIncr [var, "1"]
tclIncr [var, incr]
    = do v1 <- get >>= lookupVar var
	       >>= checkIntegerString
         v2 <- checkIntegerString incr
         get >>= setVar var (show $ v1 + v2)
         
tclIncr _
    = tclWrongArgs "incr varName ?increment?"

-- ------------------------------------------------------------

tclPuts :: TclCommand e s
tclPuts l
    = do (nnl, l1) <- tclOption1 "-nonewline" False True l
         tclPuts' nnl l1
         return ""
      where
        tclPuts' nnl [s]
            = tclPuts' nnl ["stdout", s]

        tclPuts' nnl [cn, s]
            = do h <- get >>= lookupChannel cn
                 liftIOE $ ( if nnl
                             then hPutStr
                             else hPutStrLn) h s

        tclPuts' _ _
            = tclWrongArgs "puts ?-nonewline? ?channelId? string"

-- ------------------------------------------------------------

tclReturn :: TclCommand e s
tclReturn l
    = do (rc, l1) <- tclOption2 "-code" 2 checkReturnCode l
         tclReturn' rc l1
    where
      tclReturn' rc []
          = tclReturn' rc [""]
      tclReturn' rc l1@[v]
          | rc == 1
              = tclError l1
          | rc == 2
              = throwError $ tclReturnExc v
          | rc == 3
              = tclBreak []
          | rc == 4
              = tclContinue []
          | otherwise
              = throwError $ tclOtherExc rc v
      tclReturn' _ _
          = tclWrongArgs "return ?-code code? ?result?"

checkReturnCode :: String -> TclEval e s Int
checkReturnCode s
    = checkArg ("bad completion code "
		++ show s
		++ ": must be ok, error, return, break, continue, or an integer"
	       ) rc2i s
    where
    rc2i v
	= lookup v (zip ["ok", "error", "return", "break", "continue"] [0..])
          `mplus`
          toInt v

-- ------------------------------------------------------------

tclSet	:: TclCommand e s
tclSet [n]
    = get >>= lookupVar n

tclSet [n, v]
    = get >>= setVar n v

tclSet _
    = tclWrongArgs "set varName ?newValue?"

-- ------------------------------------------------------------
