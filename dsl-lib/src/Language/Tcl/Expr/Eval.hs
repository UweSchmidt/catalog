module Language.Tcl.Expr.Eval
where

import Control.Monad
import Control.Applicative

import Data.Char        ( isLetter )
import Data.Maybe      	( isJust )

import           Language.Tcl.Core
import           Language.Tcl.Value
import           Language.Tcl.CheckArgs          	( checkBooleanValue
							, checkBooleanString
                                                        )
import           Language.Tcl.Expr.AbstractSyntax
import qualified Language.Tcl.Expr.Parser               as P

-- ------------------------------------------------------------

trueExpr  = TConst . mkI $ 1
falseExpr = TConst . mkI $ 0

eval :: TclExpr -> TclEval e s Value
eval (TConst v)
    = return v

eval (TExpr op [e1])
    = eval e1 >>= app1 op

eval (TExpr "&&" [e1, e2])
    = do b <- eval e1 >>=  checkBooleanValue
	 if b
	    then eval e2
		     >>= checkBooleanValue
		     >>= return . b2i
	    else return . mkI $ 0

eval (TExpr "||" [e1, e2])
    = do b <- eval e1 >>=  checkBooleanValue
	 if b
	    then return . mkI $ 1
	    else eval e2
		     >>= checkBooleanValue
		     >>= return . b2i

eval (TExpr op [e1, e2])
    = do v1 <- eval e1
         v2 <- eval e2
         app2 op v1 v2

eval (TExpr "?:" [e1, e2, e3])
    = do b <- eval e1 >>= checkBooleanValue
         if b
            then eval e2
            else eval e3

eval (TExpr op _el)
    = tclSyn $ op ++ " not supported"

-- ------------------------------------------------------------
--
-- unary expr evaluation

fct1I :: [(String, Integer -> TclEval e s Value)]
fct1I
    = [ ("+",   plus1I)
      , ("-",   minus1I)
      , ("!",   neg1I)
      , ("abs", absI)
      ]
    where
    mki = return . mkI
    mkb = return . b2i

    plus1I   = mki
    minus1I  = mki . (0 - )
    neg1I    = mkb . (== 0)
    absI   x = mki $ if x < 0 then (0 - x) else x 

fct1D :: [(String, Double -> TclEval e s Value)]
fct1D
    = [ ("+",   plus1D)
      , ("-",   minus1D)
      , ("!",   neg1D)
      , ("abs", absD)
      ]
    where
    mkd = return . mkD
    mkb = return . b2i

    plus1D   = mkd
    minus1D  = mkd . (0 - )
    neg1D    = mkb . (== 0)
    absD   x = mkd $ if x < 0 then (0 - x) else x 

app1Err op v
    | isLetter . head $ op
        = tclSyn $ op ++ "(" ++ show v ++ ")"
    | otherwise
        = tclSyn $ unwords [op, show v]

app1 :: String -> Value -> TclEval e s Value
app1 op v1
    | isI v1 = do
	       x1 <- selI v1
	       maybe (tclThrowError $ "unary operator/function " ++ show op ++ " not supported for integers")
		     ($ x1)
		     $ lookup op fct1I

    | isD v1 = do
	       x1 <- selD v1
	       maybe (tclThrowError $ "unary operator/function " ++ show op ++ " not supported for integers")
		     ($ x1)
		     $ lookup op fct1D
    | otherwise = case castArg v1 of
		  Nothing
		      -> app1Err op v1
		  Just y1
		      -> app1 op y1

castArg :: Value -> Maybe Value
castArg x
    = s2i x	-- only implicit cast from string booleans to integers are performed

-- ------------------------------------------------------------
--
-- binary expr evaluation

fct2I :: [(String, Integer -> Integer -> TclEval e s Value)]
fct2I
    = [ ("+",  plusI)
      , ("-",  minusI)
      , ("*",  multI)
      , ("/",  divI)
      , ("%",  modI)
      , ("==", eqI)
      , ("!=", neI)
      , (">",  grI)
      , (">=", geI)
      , ("<",  ltI)
      , ("<=", leI)
      ]
    where
    mki = return . mkI
    mkb = return . b2i

    db0 = tclThrowError "divide by zero"

    plusI  x1 x2 = mki $ x1 + x2
    minusI x1 x2 = mki $ x1 - x2
    multI  x1 x2 = mki $ x1 * x2
    divI  _x1 0  = db0
    divI   x1 x2 = mki $ x1 `div` x2
    modI  _x1 0  = db0
    modI   x1 x2 = mki $ x1 `mod` x2
    eqI    x1 x2 = mkb $ x1 == x2
    neI    x1 x2 = mkb $ x1 == x2
    grI    x1 x2 = mkb $ x1 >  x2
    geI    x1 x2 = mkb $ x1 >= x2
    ltI    x1 x2 = mkb $ x1 <  x2
    leI    x1 x2 = mkb $ x1 <= x2

fct2D :: [(String, Double -> Double -> TclEval e s Value)]
fct2D
    = [ ("+",  plusD)
      , ("-",  minusD)
      , ("*",  multD)
      , ("/",  divD)
      , ("==", eqD)
      , ("!=", neD)
      , (">",  grD)
      , (">=", geD)
      , ("<",  ltD)
      , ("<=", leD)
      ]
    where
    mkd = return . mkD
    mkb = return . b2i

    db0 = tclThrowError "divide by zero with floats"

    plusD  x1 x2 = mkd $ x1 +  x2
    minusD x1 x2 = mkd $ x1 -  x2
    multD  x1 x2 = mkd $ x1 *  x2
    divD  _x1 0  = db0
    divD   x1 x2 = mkd $ x1 /  x2
    eqD    x1 x2 = mkb $ x1 == x2
    neD    x1 x2 = mkb $ x1 == x2
    grD    x1 x2 = mkb $ x1 >  x2
    geD    x1 x2 = mkb $ x1 >= x2
    ltD    x1 x2 = mkb $ x1 <  x2
    leD    x1 x2 = mkb $ x1 <= x2


fct2S :: [(String, String -> String -> TclEval e s Value)]
fct2S
    = [ ("==", eqS)
      , ("!=", neS)
      , (">",  grS)
      , (">=", geS)
      , ("<",  ltS)
      , ("<=", leS)
      ]
    where
    mkb = return . b2i

    eqS    x1 x2 = mkb $ x1 == x2
    neS    x1 x2 = mkb $ x1 == x2
    grS    x1 x2 = mkb $ x1 >  x2
    geS    x1 x2 = mkb $ x1 >= x2
    ltS    x1 x2 = mkb $ x1 <  x2
    leS    x1 x2 = mkb $ x1 <= x2

app2 :: String -> Value -> Value -> TclEval e s Value
app2 op v1 v2
    | isI v1 && isI v2 = do
			 x1 <- selI v1
			 x2 <- selI v2
			 maybe (tclThrowError $ "binary operator " ++ show op ++ " not supported for integers")
			       (\ f -> f x1 x2)
			       $ lookup op fct2I
    | isD v1 && isD v2 = do
			 x1 <- selD v1
			 x2 <- selD v2
			 maybe (tclThrowError $ "binary operator " ++ show op ++ " not supported for floats")
			       (\ f -> f x1 x2)
			       $ lookup op fct2D
    | isS v1 && isS v2 = do
			 x1 <- selS v1
			 x2 <- selS v2
			 maybe (tclThrowError $ "binary operator " ++ show op ++ " not supported for floats")
			       (\ f -> f x1 x2)
			       $ lookup op fct2S
    | otherwise        = case castArgs v1 v2 of
			 Nothing
			     -> app2Syn op v1 v2	-- no casts possible: issue error
			 Just (y1, y2)
			     -> app2 op y1 y2		-- try again with one arg implicitly casted


castArgs :: Value -> Value -> Maybe (Value, Value)
castArgs x y
    | isJust r		= mzero
    | otherwise		= ( s2i y >>= eqarg x )		-- try string to int cast for y
                          `mplus`
                          ( s2i x >>= flip eqarg y )	-- try string to int cast for x
                          `mplus`
                          ( i2d y >>= eqarg x )		-- try int to double cast for y
                          `mplus`
                          ( i2d x >>= flip eqarg y )	-- try int to double cast for x
                          `mplus`
                          ( i2s y >>= eqarg x )		-- try int to string cast for y
                          `mplus`
                          ( i2s x >>= flip eqarg y )	-- try int to string cast for x
                          `mplus`
                          ( d2s y >>= eqarg x )		-- try double to string cast for y
                          `mplus`
                          ( d2s x >>= flip eqarg y )	-- try double to string cast for x
    where
      r = eqarg x y

-- ------------------------------------------------------------

app2Syn, app2Type :: String -> Value -> Value -> TclEval e s r

app2Syn    = app2Err tclSyn
app2Type   = app2Err tclType

app2Err :: (Show a1, Show a) => (String -> t) -> String -> a -> a1 -> t
app2Err err op v1 v2
    | isLetter . head $ op
        = err $ op ++ "(" ++ show v1 ++ "," ++ show v2 ++ ")"
    | otherwise
        = err $ unwords [show v1, op, show v2]

tclSyn, tclType :: String -> TclEval e s r

tclSyn	= tclExprErr "syntax error in expression: "
tclType	= tclExprErr "wrong argument types in expression: "

tclExprErr :: String -> String -> TclEval e s r
tclExprErr msg
    = tclThrowError . (msg ++) . show

-- ------------------------------------------------------------

parseTclExpr	:: String -> TclEval e s TclExpr
parseTclExpr s
    = case (P.parseTclExpr s) of
        Left err
            -> tclThrowError $ show err
        Right l
            -> return l

evalTclExpr :: String -> TclEval e s Value
evalTclExpr s
    = parseTclExpr s >>= eval

-- ------------------------------------------------------------
