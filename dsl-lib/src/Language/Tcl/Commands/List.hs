module Language.Tcl.Commands.List
    ( tclList
    , tclLappend
    , tclLindex
    , tclLinsert
    , tclLlength
    )
where

import Control.Applicative              ( (<$>) )
import Control.Monad.RWS

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.CheckArgs
import Language.Tcl.Expr.Eval           ( evalTclIndexExpr
                                        )

-- ------------------------------------------------------------

tclList :: TclCommand e s
tclList
    = return . mkL

-- ------------------------------------------------------------

tclLappend :: TclCommand e s
tclLappend (var : values)
    = do val <- (get >>= lookupVar varName)
                `mplus`
                return mempty
         list <- lconcat values
         res  <- lappend val list
         get >>= setVar varName res
    where
      varName = selS var

tclLappend _
    = tclWrongArgs "lappend varName ?value value value ...?"

-- ------------------------------------------------------------

tclLindex :: TclCommand e s
tclLindex (list : [])
    = tclLindex (list : mempty : [])
tclLindex (list : indices@(_:_:_))
    = tclLindex (list : mkL indices: [])
tclLindex (list : indexList : [])
    = checkListValue indexList
      >>= lindex list
    where
      lindex l []
          = return l
      lindex l (i : ixs)
          = do l'      <- checkListValue l
               let len = length l'
               i'      <- fromInteger <$> evalTclIndexExpr (toInteger (len -1)) i
               let res = if i' < 0 || i' > len
                         then mempty
                         else l' !! i'
               lindex res ixs
                  
tclLindex _
    = tclWrongArgs "lindex list ?index...?"

-- ------------------------------------------------------------

tclLinsert :: TclCommand e s
tclLinsert (list' : index' : values@(_ : _))
    = do list    <- checkListValue list'
         let len =  length list
         let val =  mkL values
         index   <- fromInteger <$> evalTclIndexExpr (toInteger len) index'
         case (index `max` 0) `min` len of
           ix | ix == 0   ->     val  `lappend` mkL list
              | ix == len -> mkL list `lappend` val
              | otherwise -> let (v1, v2) = splitAt ix list in
                             do rest <- val `lappend` mkL v2
                                mkL v1 `lappend` rest
tclLinsert _
    = tclWrongArgs "linsert list index element ?element element ...?"

-- ------------------------------------------------------------

tclLlength :: TclCommand e s
tclLlength (list : [])
    = checkListValue list
      >>= (return . mkI . toInteger . length)

tclLlength _
    = tclWrongArgs "llength list"

-- ------------------------------------------------------------
