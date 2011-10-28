{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude, ExistentialQuantification #-}

#if MIN_VERSION_base(4,3,0)
{-# LANGUAGE RankNTypes #-} -- for mask
#endif

{- |
Module      :  Control.Exception.Control
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
Portability :  non-portable (extended exceptions)

This is a wrapped version of @Control.Exception@ with types generalized
from @IO@ to all monads in 'MonadControlIO'.
-}

module Control.Exception.Control
    ( module Control.Exception

      -- * Throwing exceptions
    , throwIO, ioError

      -- * Catching exceptions
      -- ** The @catch@ functions
    , catch, catches, Handler(..), catchJust

      -- ** The @handle@ functions
    , handle, handleJust

      -- ** The @try@ functions
    , try, tryJust

      -- ** The @evaluate@ function
    , evaluate

      -- * Asynchronous Exceptions
      -- ** Asynchronous exception control
      -- |The following functions allow a thread to control delivery of
      -- asynchronous exceptions during a critical region.
#if MIN_VERSION_base(4,3,0)
    , mask, mask_
    , uninterruptibleMask, uninterruptibleMask_
    , getMaskingState
#else
    , block, unblock
#endif

#if !MIN_VERSION_base(4,4,0)
    , blocked
#endif
      -- * Brackets
    , bracket, bracket_, bracketOnError

      -- * Utilities
    , finally, onException
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function   ( ($) )
import Data.Either     ( Either(Left, Right), either )
import Data.Maybe      ( Maybe )
import Control.Monad   ( Monad, (>>=), return, liftM )
import System.IO.Error ( IOError )

#if MIN_VERSION_base(4,3,0) || defined (__HADDOCK__)
import System.IO       ( IO )
#endif

#if __GLASGOW_HASKELL__ < 700
import Control.Monad   ( fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.IO.Class ( MonadIO, liftIO )

import Control.Exception hiding
    ( throwIO, ioError
    , catch, catches, Handler(..), catchJust
    , handle, handleJust
    , try, tryJust
    , evaluate
#if MIN_VERSION_base(4,3,0)
    , mask, mask_
    , uninterruptibleMask, uninterruptibleMask_
    , getMaskingState
#else
    , block, unblock
#endif
#if !MIN_VERSION_base(4,4,0)
    , blocked
#endif
    , bracket, bracket_, bracketOnError
    , finally, onException
    )
import qualified Control.Exception as E

#if !MIN_VERSION_base(4,4,0)
import Data.Bool ( Bool )
#endif

-- from monad-control (this package):
import Control.Monad.Trans.Control ( MonadControlIO, controlIO, liftIOOp_ )
#if MIN_VERSION_base(4,3,0) || defined (__HADDOCK__)
import Control.Monad.Trans.Control ( liftIOOp )
#endif

--------------------------------------------------------------------------------
-- * Throwing exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.throwIO'.
throwIO ∷ (MonadIO m, Exception e) ⇒ e → m α
throwIO = liftIO ∘ E.throwIO

-- |Generalized version of 'E.ioError'.
ioError ∷ MonadIO m ⇒ IOError → m α
ioError = liftIO ∘ E.ioError


--------------------------------------------------------------------------------
-- * Catching exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.catch'.
{-# INLINABLE catch #-}
catch ∷ (MonadControlIO m, Exception e)
      ⇒ m α       -- ^ The computation to run
      → (e → m α) -- ^ Handler to invoke if an exception is raised
      → m α
catch a handler = controlIO $ \runInIO →
                    E.catch (runInIO a)
                            (\e → runInIO $ handler e)

-- |Generalized version of 'E.catches'.
{-# INLINABLE catches #-}
catches ∷ MonadControlIO m ⇒ m α → [Handler m α] → m α
catches a handlers = controlIO $ \runInIO →
                       E.catches (runInIO a)
                                 [ E.Handler $ \e → runInIO $ handler e
                                 | Handler handler ← handlers
                                 ]

-- |Generalized version of 'E.Handler'.
data Handler m α = ∀ e. Exception e ⇒ Handler (e → m α)

-- |Generalized version of 'E.catchJust'.
{-# INLINABLE catchJust #-}
catchJust ∷ (MonadControlIO m, Exception e)
          ⇒ (e → Maybe β) -- ^ Predicate to select exceptions
          → m α           -- ^ Computation to run
          → (β → m α)     -- ^ Handler
          → m α
catchJust p a handler = controlIO $ \runInIO →
                          E.catchJust p
                                      (runInIO a)
                                      (\e → runInIO (handler e))


--------------------------------------------------------------------------------
--  ** The @handle@ functions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.handle'.
{-# INLINABLE handle #-}
handle ∷ (MonadControlIO m, Exception e) ⇒ (e → m α) → m α → m α
handle handler a = controlIO $ \runInIO →
                     E.handle (\e → runInIO (handler e))
                              (runInIO a)

-- |Generalized version of 'E.handleJust'.
{-# INLINABLE handleJust #-}
handleJust ∷ (MonadControlIO m, Exception e)
           ⇒ (e → Maybe β) → (β → m α) → m α → m α
handleJust p handler a = controlIO $ \runInIO →
                           E.handleJust p (\e → runInIO (handler e))
                                          (runInIO a)


--------------------------------------------------------------------------------
-- ** The @try@ functions
--------------------------------------------------------------------------------

sequenceEither ∷ Monad m ⇒ Either e (m α) → m (Either e α)
sequenceEither = either (return ∘ Left) (liftM Right)

-- |Generalized version of 'E.try'.
{-# INLINABLE try #-}
try ∷ (MonadControlIO m, Exception e) ⇒ m α → m (Either e α)
try = liftIOOp_ (liftM sequenceEither ∘ E.try)

-- |Generalized version of 'E.tryJust'.
{-# INLINABLE tryJust #-}
tryJust ∷ (MonadControlIO m, Exception e) ⇒
           (e → Maybe β) → m α → m (Either β α)
tryJust p = liftIOOp_ (liftM sequenceEither ∘ E.tryJust p)


--------------------------------------------------------------------------------
-- ** The @evaluate@ function
--------------------------------------------------------------------------------

-- |Generalized version of 'E.evaluate'.
evaluate ∷ MonadIO m ⇒ α → m α
evaluate = liftIO ∘ E.evaluate


--------------------------------------------------------------------------------
-- ** Asynchronous exception control
--------------------------------------------------------------------------------

#if MIN_VERSION_base(4,3,0)
-- |Generalized version of 'E.mask'.
{-# INLINABLE mask #-}
mask ∷ MonadControlIO m ⇒ ((∀ α. m α → m α) → m β) → m β
mask = liftIOOp E.mask ∘ liftRestore

liftRestore ∷ MonadControlIO m
            ⇒ ((∀ α.  m α →  m α) → β)
            → ((∀ α. IO α → IO α) → β)
liftRestore f restore = f $ liftIOOp_ restore

-- |Generalized version of 'E.mask_'.
{-# INLINABLE mask_ #-}
mask_ ∷ MonadControlIO m ⇒ m α → m α
mask_ = liftIOOp_ E.mask_

-- |Generalized version of 'E.uninterruptibleMask'.
{-# INLINABLE uninterruptibleMask #-}
uninterruptibleMask ∷ MonadControlIO m ⇒ ((∀ α. m α → m α) → m β) → m β
uninterruptibleMask = liftIOOp E.uninterruptibleMask ∘ liftRestore

-- |Generalized version of 'E.uninterruptibleMask_'.
{-# INLINABLE uninterruptibleMask_ #-}
uninterruptibleMask_ ∷ MonadControlIO m ⇒ m α → m α
uninterruptibleMask_ = liftIOOp_ E.uninterruptibleMask_

-- |Generalized version of 'E.getMaskingState'.
getMaskingState ∷ MonadIO m ⇒ m MaskingState
getMaskingState = liftIO E.getMaskingState
#else
-- |Generalized version of 'E.block'.
{-# INLINABLE block #-}
block ∷ MonadControlIO m ⇒ m α → m α
block = liftIOOp_ E.block

-- |Generalized version of 'E.unblock'.
{-# INLINABLE unblock #-}
unblock ∷ MonadControlIO m ⇒ m α → m α
unblock = liftIOOp_ E.unblock
#endif

#if !MIN_VERSION_base(4,4,0)
-- | Generalized version of 'E.blocked'.
-- returns @True@ if asynchronous exceptions are blocked in the
-- current thread.
blocked ∷ MonadIO m ⇒ m Bool
blocked = liftIO E.blocked
#endif


--------------------------------------------------------------------------------
-- * Brackets
--------------------------------------------------------------------------------

-- |Generalized version of 'E.bracket'.  Note, any monadic side
-- effects in @m@ of the \"release\" computation will be discarded; it
-- is run only for its side effects in @IO@.
--
-- Note that when your @acquire@ and @release@ computations are of type 'IO'
-- it will be more efficient to write:
--
-- @'liftIOOp' ('E.bracket' acquire release)@
{-# INLINABLE bracket #-}
bracket ∷ MonadControlIO m
        ⇒ m α       -- ^ computation to run first (\"acquire resource\")
        → (α → m β) -- ^ computation to run last (\"release resource\")
        → (α → m γ) -- ^ computation to run in-between
        → m γ
bracket before after thing = controlIO $ \runInIO →
                               E.bracket (runInIO before)
                                         (\m → runInIO $ m >>= after)
                                         (\m → runInIO $ m >>= thing)

-- |Generalized version of 'E.bracket_'.  Note, any monadic side
-- effects in @m@ of /both/ the \"acquire\" and \"release\"
-- computations will be discarded.  To keep the monadic side effects
-- of the \"acquire\" computation, use 'bracket' with constant
-- functions instead.
--
-- Note that when your @acquire@ and @release@ computations are of type 'IO'
-- it will be more efficient to write:
--
-- @'liftIOOp_' ('E.bracket_' acquire release)@
{-# INLINABLE bracket_ #-}
bracket_ ∷ MonadControlIO m
         ⇒ m α -- ^ computation to run first (\"acquire resource\")
         → m β -- ^ computation to run last (\"release resource\")
         → m γ -- ^ computation to run in-between
         → m γ
bracket_ before after thing = controlIO $ \runInIO →
                                E.bracket_ (runInIO before)
                                           (runInIO after)
                                           (runInIO thing)

-- |Generalized version of 'E.bracketOnError'.  Note, any monadic side
-- effects in @m@ of the \"release\" computation will be discarded.
--
-- Note that when your @acquire@ and @release@ computations are of type 'IO'
-- it will be more efficient to write:
--
-- @'liftIOOp' ('E.bracketOnError' acquire release)@
{-# INLINABLE bracketOnError #-}
bracketOnError ∷ MonadControlIO m
               ⇒ m α       -- ^ computation to run first (\"acquire resource\")
               → (α → m β) -- ^ computation to run last (\"release resource\")
               → (α → m γ) -- ^ computation to run in-between
               → m γ
bracketOnError before after thing = controlIO $ \runInIO →
                                      E.bracketOnError (runInIO before)
                                                       (\m → runInIO $ m >>= after)
                                                       (\m → runInIO $ m >>= thing)


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- |Generalized version of 'E.finally'.  Note, any monadic side
-- effects in @m@ of the \"afterward\" computation will be discarded.
{-# INLINABLE finally #-}
finally ∷ MonadControlIO m
        ⇒ m α -- ^ computation to run first
        → m β -- ^ computation to run afterward (even if an exception was raised)
        → m α
finally a sequel = controlIO $ \runInIO →
                     E.finally (runInIO a)
                               (runInIO sequel)

-- |Generalized version of 'E.onException'.  Note, any monadic side
-- effects in @m@ of the \"afterward\" computation will be discarded.
{-# INLINABLE onException #-}
onException ∷ MonadControlIO m ⇒ m α → m β → m α
onException m what = controlIO  $ \runInIO →
                       E.onException (runInIO m)
                                     (runInIO what)
