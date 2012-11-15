module Language.Lua.VM
    ( module Language.Lua.VM
    , module Language.Lua.VM.Types
    , module Language.Lua.VM.Value
    , module Language.Lua.VM.Core
    , module Language.Lua.VM.CoreFct
    )
where

-- import Control.Monad.Error
-- import Control.Monad.Reader
import Control.Monad.State

-- import Language.Lua.VM.Instr
import Language.Lua.VM.Types
import Language.Lua.VM.Value
import Language.Lua.VM.Core
import Language.Lua.VM.CoreFct

-- ------------------------------------------------------------

runLua :: LuaAction res -> LuaModule -> LuaState -> IO (Either LuaError res, LuaState)
runLua act prog s0
    = runEval (initLuaEnv >> loadCode prog >> act) emptyLuaEnv s0

initLua :: IO (Either LuaError (), LuaState)
initLua
    = runEval (initLuaEnv >> return ()) emptyLuaEnv emptyLuaState

-- ------------------------------------------------------------

initLuaEnv :: LuaAction ()
initLuaEnv
    = do openEnv        -- create global env table
         ge <- gets theCurrEnv
         writeVariable (S "_G") (T . head . theEnv $ ge) ge     -- insert the global env table into itself
                                                                -- under name "_G"
         addCoreFunctions                                       -- add the core native functions
         addVMFunctions                                         -- add native functions for observing the VM

-- ------------------------------------------------------------
