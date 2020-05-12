{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

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
import Polysemy.State

import           Data.Text    (Text)
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
    (\ c -> case c of
        Consume v -> do
          embed $ toIO v
        )


-- | Run a 'Consume Text' effect by writing to stdout/stderr

writelnToStdout :: Member (Embed IO) r
                => InterpreterFor (Consume Text) r
writelnToStdout = consumeIO T.putStrLn

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
    (\ c -> case c of
        Consume v -> modify' (<> f v)
    )

{-# INLINE runConsumeMonoid #-}


-- | Run a 'Consume' effect by applying an IO cmd.

consumeNull :: InterpreterFor (Consume v) r
consumeNull =
  interpret
    (\ c -> case c of
        Consume _v -> return ()
        )

{-# INLINE consumeNull #-}

------------------------------------------------------------------------------
