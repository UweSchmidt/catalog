{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------

module Polysemy.Consume
  ( -- * Effect
    Consume (..)

    -- * Actions
  , consume
  , writeln

    -- * Interpretations
  , consumeIO          -- perform an arbitrary IO cmd to process value
  , writelnToStdout    -- writeln a Text to stdout
  , runConsumeMonoid   -- collect values in a monoid
  , consumeNull        -- ignore everything
  )
where

import Polysemy
       ( InterpreterFor
       , Member
       , Sem
       , Embed
       , makeSem
       , reinterpret
       , interpret
       , embed
       )
import Polysemy.State
       ( runState
       , modify'
       )

import System.IO
       ( stdout
       , hFlush
       )
import Data.Text
       ( Text )

import qualified Data.Text.IO as T

------------------------------------------------------------------------------

data Consume v m a where
  Consume :: v -> Consume v m ()

makeSem ''Consume

writeln :: Member (Consume Text) r
        => Text -> Sem r ()
writeln = consume @Text

------------------------------------------------------------------------------
--
-- | Run a 'Consume' effect by applying an IO cmd.

consumeIO :: Member (Embed IO) r
          => (v -> IO ())
          -> InterpreterFor (Consume v) r
consumeIO toIO =
  interpret
    (\ case
        Consume v -> embed $ toIO v
    )


-- | Run a 'Consume Text' effect by writing to stdout/stderr

writelnToStdout :: Member (Embed IO) r
                => InterpreterFor (Consume Text) r
writelnToStdout = consumeIO (\ t -> T.putStrLn t >> hFlush stdout)

{-# INLINE consumeIO #-}
{-# INLINE writelnToStdout #-}

------------------------------------------------------------------------------
--
-- | Run a 'Consume' effect by transforming it into a monoid.

runConsumeMonoid
    :: Monoid m
    => (v -> m)
    -> Sem (Consume v ': r) a
    -> Sem r (m, a)

runConsumeMonoid f =
  runState mempty . reinterpret
    (\ case
        Consume v -> modify' (<> f v)
    )

{-# INLINE runConsumeMonoid #-}


-- | Run a 'Consume' effect by applying an IO cmd.

consumeNull :: InterpreterFor (Consume v) r
consumeNull =
  interpret
    (\ case
        Consume _v -> return ()
    )

{-# INLINE consumeNull #-}

------------------------------------------------------------------------------
