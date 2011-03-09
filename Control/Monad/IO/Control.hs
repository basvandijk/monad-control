{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, RankNTypes #-}

{- |
Module      :  Control.Monad.IO.Control
Copyright   :  © Bas van Dijk, Anders Kaseorg, 2011
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
Portability :  Requires RankNTypes

This module defines the class 'MonadControlIO' of 'IO'-based monads into
which control operations on 'IO' (such as exception catching; see
"Control.Exception.Control") can be lifted.

'liftIOOp' and 'liftIOOp_' enable convenient lifting of two common
special cases of control operation types.
-}

module Control.Monad.IO.Control
    ( MonadControlIO(..)
    , controlIO

    , liftIOOp
    , liftIOOp_
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( ($) )
import Data.Monoid   ( Monoid )
import System.IO     ( IO )
import Control.Monad ( join )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.IO.Class       ( MonadIO )

import Control.Monad.Trans.Identity ( IdentityT )
import Control.Monad.Trans.List     ( ListT )
import Control.Monad.Trans.Maybe    ( MaybeT )
import Control.Monad.Trans.Error    ( ErrorT, Error )
import Control.Monad.Trans.Reader   ( ReaderT )
import Control.Monad.Trans.State    ( StateT )
import Control.Monad.Trans.Writer   ( WriterT )
import Control.Monad.Trans.RWS      ( RWST )

import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST )

-- from monad-control (this package):
import Control.Monad.Trans.Control ( idLiftControl
                                   , liftLiftControlBase
                                   , RunInBase
                                   )


--------------------------------------------------------------------------------
-- MonadControlIO
--------------------------------------------------------------------------------

{-|
@MonadControlIO@ is the class of 'IO'-based monads supporting an
extra operation 'liftControlIO', enabling control operations on 'IO' to be
lifted into the monad.
-}
class MonadIO m ⇒ MonadControlIO m where
  {-|
  @liftControlIO@ is a version of @liftControl@ that operates through an
  arbitrary stack of monad transformers directly to an inner 'IO'
  (analagously to how 'liftIO' is a version of @lift@).  So it can
  be used to lift control operations on 'IO' into any
  monad in 'MonadControlIO'.  For example:

  @
  foo :: 'IO' a -> 'IO' a
  foo' :: 'MonadControlIO' m => m a -> m a
  foo' a = 'controlIO' $ \runInIO ->    -- runInIO :: m a -> 'IO' (m a)
             foo $ runInIO a         -- uses foo :: 'IO' (m a) -> 'IO' (m a)
  @

  Instances should satisfy similar laws as the 'MonadIO' laws:

  @liftControlIO . const . return = return@

  @liftControlIO (const (m >>= f)) = liftControlIO (const m) >>= liftControlIO . const . f@

  Additionally instances should satisfy:

  @'controlIO' $ \\runInIO -> runInIO m = m@
  -}
  liftControlIO ∷ (RunInBase m IO → IO α) → m α

-- | An often used composition: @controlIO = 'join' . 'liftControlIO'@
controlIO ∷ MonadControlIO m ⇒ (RunInBase m IO → IO (m α)) → m α
controlIO = join ∘ liftControlIO


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadControlIO IO where
    liftControlIO = idLiftControl

instance MonadControlIO m ⇒ MonadControlIO (IdentityT m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (ListT m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (MaybeT m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Error e, MonadControlIO m) ⇒ MonadControlIO (ErrorT e m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (ReaderT r m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (StateT s m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (Strict.StateT s m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (WriterT w m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (Strict.WriterT w m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (RWST r w s m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (Strict.RWST r w s m) where
    liftControlIO = liftLiftControlBase liftControlIO


--------------------------------------------------------------------------------
-- Convenient lifting of two common special cases of control operation types
--------------------------------------------------------------------------------

{-|
@liftIOOp@ is a particular application of 'liftControlIO' that allows
lifting control operations of type @(a -> 'IO' b) -> 'IO' b@
(e.g. @alloca@, @withMVar v@) to
@'MonadControlIO' m => (a -> m b) -> m b@.

@liftIOOp f = \\g -> 'controlIO' $ \runInIO -> f $ runInIO . g@
-}
liftIOOp ∷ MonadControlIO m
         ⇒ ((α → IO (m β)) → IO (m γ))
         → ((α →     m β)  →     m γ)
liftIOOp f = \g → controlIO $ \runInIO → f $ runInIO ∘ g

{-|
@liftIOOp_@ is a particular application of 'liftControlIO' that allows
lifting control operations of type @'IO' a -> 'IO' a@
(e.g. @block@) to @'MonadControlIO' m => m a -> m a@.

@liftIOOp_ f = \\m -> 'controlIO' $ \runInIO -> f $ runInIO m@
-}
liftIOOp_ ∷ MonadControlIO m
          ⇒ (IO (m α) → IO (m β))
          → (    m α →      m β)
liftIOOp_ f = \m → controlIO $ \runInIO → f $ runInIO m


-- The End ---------------------------------------------------------------------
