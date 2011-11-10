{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , ExistentialQuantification
           , FlexibleContexts
  #-}

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

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

-- from monad-control (this package):
import Control.Monad.Trans.Control ( MonadBaseControl, StBase
                                   , liftBaseControl, restore
                                   , controlBase, liftBaseOp_
                                   )
#if MIN_VERSION_base(4,3,0) || defined (__HADDOCK__)
import Control.Monad.Trans.Control ( liftBaseOp )
#endif

--------------------------------------------------------------------------------
-- * Throwing exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.throwIO'.
throwIO ∷ (MonadBase IO m, Exception e) ⇒ e → m α
throwIO = liftBase ∘ E.throwIO

-- |Generalized version of 'E.ioError'.
ioError ∷ MonadBase IO m ⇒ IOError → m α
ioError = liftBase ∘ E.ioError


--------------------------------------------------------------------------------
-- * Catching exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.catch'.
{-# INLINABLE catch #-}
catch ∷ (MonadBaseControl IO m, Exception e)
      ⇒ m α       -- ^ The computation to run
      → (e → m α) -- ^ Handler to invoke if an exception is raised
      → m α
catch a handler = controlBase $ \runInIO →
                    E.catch (runInIO a)
                            (\e → runInIO $ handler e)

-- |Generalized version of 'E.catches'.
{-# INLINABLE catches #-}
catches ∷ MonadBaseControl IO m ⇒ m α → [Handler m α] → m α
catches a handlers = controlBase $ \runInIO →
                       E.catches (runInIO a)
                                 [ E.Handler $ \e → runInIO $ handler e
                                 | Handler handler ← handlers
                                 ]

-- |Generalized version of 'E.Handler'.
data Handler m α = ∀ e. Exception e ⇒ Handler (e → m α)

-- |Generalized version of 'E.catchJust'.
{-# INLINABLE catchJust #-}
catchJust ∷ (MonadBaseControl IO m, Exception e)
          ⇒ (e → Maybe β) -- ^ Predicate to select exceptions
          → m α           -- ^ Computation to run
          → (β → m α)     -- ^ Handler
          → m α
catchJust p a handler = controlBase $ \runInIO →
                          E.catchJust p
                                      (runInIO a)
                                      (\e → runInIO (handler e))


--------------------------------------------------------------------------------
--  ** The @handle@ functions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.handle'.
{-# INLINABLE handle #-}
handle ∷ (MonadBaseControl IO m, Exception e) ⇒ (e → m α) → m α → m α
handle handler a = controlBase $ \runInIO →
                     E.handle (\e → runInIO (handler e))
                              (runInIO a)

-- |Generalized version of 'E.handleJust'.
{-# INLINABLE handleJust #-}
handleJust ∷ (MonadBaseControl IO m, Exception e)
           ⇒ (e → Maybe β) → (β → m α) → m α → m α
handleJust p handler a = controlBase $ \runInIO →
                           E.handleJust p (\e → runInIO (handler e))
                                          (runInIO a)

--------------------------------------------------------------------------------
-- ** The @try@ functions
--------------------------------------------------------------------------------

sequenceEither ∷ MonadBaseControl IO m ⇒ Either e (StBase m α) → m (Either e α)
sequenceEither = either (return ∘ Left) (liftM Right ∘ restore)
{-# INLINE sequenceEither #-}

-- |Generalized version of 'E.try'.
{-# INLINABLE try #-}
try ∷ (MonadBaseControl IO m, Exception e) ⇒ m α → m (Either e α)
try m = liftBaseControl (\runInIO → E.try (runInIO m)) >>= sequenceEither

-- |Generalized version of 'E.tryJust'.
{-# INLINABLE tryJust #-}
tryJust ∷ (MonadBaseControl IO m, Exception e) ⇒ (e → Maybe β) → m α → m (Either β α)
tryJust p m = liftBaseControl (\runInIO → E.tryJust p (runInIO m)) >>= sequenceEither


--------------------------------------------------------------------------------
-- ** The @evaluate@ function
--------------------------------------------------------------------------------

-- |Generalized version of 'E.evaluate'.
evaluate ∷ MonadBase IO m ⇒ α → m α
evaluate = liftBase ∘ E.evaluate


--------------------------------------------------------------------------------
-- ** Asynchronous exception control
--------------------------------------------------------------------------------

#if MIN_VERSION_base(4,3,0)
-- |Generalized version of 'E.mask'.
{-# INLINABLE mask #-}
mask ∷ MonadBaseControl IO m ⇒ ((∀ α. m α → m α) → m β) → m β
mask = liftBaseOp E.mask ∘ liftRestore

liftRestore ∷ MonadBaseControl IO m
            ⇒ ((∀ α.  m α →  m α) → β)
            → ((∀ α. IO α → IO α) → β)
liftRestore f r = f $ liftBaseOp_ r

-- |Generalized version of 'E.mask_'.
{-# INLINABLE mask_ #-}
mask_ ∷ MonadBaseControl IO m ⇒ m α → m α
mask_ = liftBaseOp_ E.mask_

-- |Generalized version of 'E.uninterruptibleMask'.
{-# INLINABLE uninterruptibleMask #-}
uninterruptibleMask ∷ MonadBaseControl IO m ⇒ ((∀ α. m α → m α) → m β) → m β
uninterruptibleMask = liftBaseOp E.uninterruptibleMask ∘ liftRestore

-- |Generalized version of 'E.uninterruptibleMask_'.
{-# INLINABLE uninterruptibleMask_ #-}
uninterruptibleMask_ ∷ MonadBaseControl IO m ⇒ m α → m α
uninterruptibleMask_ = liftBaseOp_ E.uninterruptibleMask_

-- |Generalized version of 'E.getMaskingState'.
getMaskingState ∷ MonadBase IO m ⇒ m MaskingState
getMaskingState = liftBase E.getMaskingState
#else
-- |Generalized version of 'E.block'.
{-# INLINABLE block #-}
block ∷ MonadBaseControl IO m ⇒ m α → m α
block = liftBaseOp_ E.block

-- |Generalized version of 'E.unblock'.
{-# INLINABLE unblock #-}
unblock ∷ MonadBaseControl IO m ⇒ m α → m α
unblock = liftBaseOp_ E.unblock
#endif

#if !MIN_VERSION_base(4,4,0)
-- | Generalized version of 'E.blocked'.
-- returns @True@ if asynchronous exceptions are blocked in the
-- current thread.
blocked ∷ MonadBase IO m ⇒ m Bool
blocked = liftBase E.blocked
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
-- @'liftBaseOp' ('E.bracket' acquire release)@
{-# INLINABLE bracket #-}
bracket ∷ MonadBaseControl IO m
        ⇒ m α       -- ^ computation to run first (\"acquire resource\")
        → (α → m β) -- ^ computation to run last (\"release resource\")
        → (α → m γ) -- ^ computation to run in-between
        → m γ
bracket before after thing = controlBase $ \runInIO →
                               E.bracket (runInIO before)
                                         (\st → runInIO $ restore st >>= after)
                                         (\st → runInIO $ restore st >>= thing)

-- |Generalized version of 'E.bracket_'.  Note, any monadic side
-- effects in @m@ of /both/ the \"acquire\" and \"release\"
-- computations will be discarded.  To keep the monadic side effects
-- of the \"acquire\" computation, use 'bracket' with constant
-- functions instead.
--
-- Note that when your @acquire@ and @release@ computations are of type 'IO'
-- it will be more efficient to write:
--
-- @'liftBaseOp_' ('E.bracket_' acquire release)@
{-# INLINABLE bracket_ #-}
bracket_ ∷ MonadBaseControl IO m
         ⇒ m α -- ^ computation to run first (\"acquire resource\")
         → m β -- ^ computation to run last (\"release resource\")
         → m γ -- ^ computation to run in-between
         → m γ
bracket_ before after thing = controlBase $ \runInIO →
                                E.bracket_ (runInIO before)
                                           (runInIO after)
                                           (runInIO thing)

-- |Generalized version of 'E.bracketOnError'.  Note, any monadic side
-- effects in @m@ of the \"release\" computation will be discarded.
--
-- Note that when your @acquire@ and @release@ computations are of type 'IO'
-- it will be more efficient to write:
--
-- @'liftBaseOp' ('E.bracketOnError' acquire release)@
{-# INLINABLE bracketOnError #-}
bracketOnError ∷ MonadBaseControl IO m
               ⇒ m α       -- ^ computation to run first (\"acquire resource\")
               → (α → m β) -- ^ computation to run last (\"release resource\")
               → (α → m γ) -- ^ computation to run in-between
               → m γ
bracketOnError before after thing =
    controlBase $ \runInIO →
      E.bracketOnError (runInIO before)
                       (\st → runInIO $ restore st >>= after)
                       (\st → runInIO $ restore st >>= thing)


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- |Generalized version of 'E.finally'.  Note, any monadic side
-- effects in @m@ of the \"afterward\" computation will be discarded.
{-# INLINABLE finally #-}
finally ∷ MonadBaseControl IO m
        ⇒ m α -- ^ computation to run first
        → m β -- ^ computation to run afterward (even if an exception was raised)
        → m α
finally a sequel = controlBase $ \runInIO →
                     E.finally (runInIO a)
                               (runInIO sequel)

-- |Generalized version of 'E.onException'.  Note, any monadic side
-- effects in @m@ of the \"afterward\" computation will be discarded.
{-# INLINABLE onException #-}
onException ∷ MonadBaseControl IO m ⇒ m α → m β → m α
onException m what = controlBase  $ \runInIO →
                       E.onException (runInIO m)
                                     (runInIO what)
