{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, FlexibleContexts #-}

{- |
Module      :  Control.Concurrent.MVar.Control
Copyright   :  Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of 'Control.Concurrent.MVar' with types generalized
from @IO@ to all monads in either 'MonadIO' or 'MonadControlIO'.
-}

module Control.Concurrent.MVar.Control
    ( MVar.MVar
    , newEmptyMVar
    , newMVar
    , takeMVar
    , putMVar
    , readMVar
    , swapMVar
    , tryTakeMVar
    , tryPutMVar
    , isEmptyMVar
    , withMVar
    , modifyMVar_
    , modifyMVar
    , addMVarFinalizer
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool )
import Data.Function ( ($) )
import Data.Maybe    ( Maybe )
import Control.Monad ( return, void )
import System.IO     ( IO )
import           Control.Concurrent.MVar  ( MVar )
import qualified Control.Concurrent.MVar as MVar

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- from monad-control (this package):
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseControl, liftBaseOp )
import Control.Exception.Control   ( mask, onException )


--------------------------------------------------------------------------------
-- * MVars
--------------------------------------------------------------------------------

-- | Generalized version of 'MVar.newEmptyMVar'.
newEmptyMVar ∷ MonadIO m ⇒ m (MVar α)
newEmptyMVar = liftIO MVar.newEmptyMVar

-- | Generalized version of 'MVar.newMVar'.
newMVar ∷ MonadIO m ⇒ α → m (MVar α)
newMVar = liftIO ∘ MVar.newMVar

-- | Generalized version of 'MVar.takeMVar'.
takeMVar ∷ MonadIO m ⇒ MVar α → m α
takeMVar = liftIO ∘ MVar.takeMVar

-- | Generalized version of 'MVar.putMVar'.
putMVar ∷ MonadIO m ⇒ MVar α → α → m ()
putMVar mv x = liftIO $ MVar.putMVar mv x

-- | Generalized version of 'MVar.readMVar'.
readMVar ∷ MonadIO m ⇒ MVar α → m α
readMVar = liftIO ∘ MVar.readMVar

-- | Generalized version of 'MVar.swapMVar'.
swapMVar ∷ MonadIO m ⇒ MVar α → α → m α
swapMVar mv x = liftIO $ MVar.swapMVar mv x

-- | Generalized version of 'MVar.tryTakeMVar'.
tryTakeMVar ∷ MonadIO m ⇒ MVar α → m (Maybe α)
tryTakeMVar = liftIO ∘ MVar.tryTakeMVar

-- | Generalized version of 'MVar.tryPutMVar'.
tryPutMVar ∷ MonadIO m ⇒ MVar α → α → m Bool
tryPutMVar mv x = liftIO $ MVar.tryPutMVar mv x

-- | Generalized version of 'MVar.isEmptyMVar'.
isEmptyMVar ∷ MonadIO m ⇒ MVar α → m Bool
isEmptyMVar = liftIO ∘ MVar.isEmptyMVar

-- | Generalized version of 'MVar.withMVar'.
withMVar ∷ MonadBaseControl m IO ⇒ MVar α → (α → m β) → m β
withMVar = liftBaseOp ∘ MVar.withMVar

-- | Generalized version of 'MVar.modifyMVar_'.
modifyMVar_ ∷ (MonadBaseControl m IO, MonadIO m) ⇒ MVar α → (α → m α) → m ()
modifyMVar_ mv f = mask $ \restore → do
                     x  ← takeMVar mv
                     x' ← restore (f x) `onException` putMVar mv x
                     putMVar mv x'

-- | Generalized version of 'MVar.modifyMVar'.
modifyMVar ∷ (MonadBaseControl m IO, MonadIO m) ⇒ MVar α → (α → m (α, β)) → m β
modifyMVar mv f = mask $ \restore → do
                    x       ← takeMVar mv
                    (x', y) ← restore (f x) `onException` putMVar mv x
                    putMVar mv x'
                    return y

-- | Generalized version of 'MVar.addMVarFinalizer'.
--
-- Note any monadic side effects in @m@ of the \"finalizer\" computation are
-- discarded.
addMVarFinalizer ∷ MonadBaseControl m IO ⇒ MVar α → m () → m ()
addMVarFinalizer mv m = liftBaseControl $ \runInIO →
                          MVar.addMVarFinalizer mv (void $ runInIO m)
