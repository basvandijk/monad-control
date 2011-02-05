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
    , blocked

      -- * Utilities
    , bracket, bracket_, bracketOnError
    , finally, onException
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function   ( ($) )
import Data.Either     ( Either(Left, Right) )
import Data.Maybe      ( Maybe )
import Data.Bool       ( Bool )
import Control.Monad   ( Monad, (>>=), return, liftM )
import System.IO.Error ( IOError )

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
    , blocked
    , bracket, bracket_, bracketOnError
    , finally, onException
    )
import qualified Control.Exception as E

-- from monad-control (this package):
import Control.Monad.IO.Control ( MonadControlIO
                                , controlIO
                                , liftIOOp_
                                )


--------------------------------------------------------------------------------
-- * Throwing exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.throwIO'.
throwIO ∷ (MonadIO m, Exception e) ⇒ e → m a
throwIO = liftIO ∘ E.throwIO

-- |Generalized version of 'E.ioError'.
ioError ∷ MonadIO m ⇒ IOError → m a
ioError = liftIO ∘ E.ioError


--------------------------------------------------------------------------------
-- * Catching exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.catch'.
catch ∷ (MonadControlIO m, Exception e)
      ⇒ m a       -- ^ The computation to run
      → (e → m a) -- ^ Handler to invoke if an exception is raised
      → m a
catch a handler = controlIO $ \runInIO →
                    E.catch (runInIO a)
                            (\e → runInIO $ handler e)

-- |Generalized version of 'E.catches'.
catches ∷ MonadControlIO m ⇒ m a → [Handler m a] → m a
catches a handlers = controlIO $ \runInIO →
                       E.catches (runInIO a)
                                 [ E.Handler $ \e → runInIO $ handler e
                                 | Handler handler ← handlers
                                 ]

-- |Generalized version of 'E.Handler'.
data Handler m a = ∀ e. Exception e ⇒ Handler (e → m a)

-- |Generalized version of 'E.catchJust'.
catchJust ∷ (MonadControlIO m, Exception e)
          ⇒ (e → Maybe b) -- ^ Predicate to select exceptions
          → m a           -- ^ Computation to run
          → (b → m a)     -- ^ Handler
          → m a
catchJust p a handler = controlIO $ \runInIO →
                          E.catchJust p
                                      (runInIO a)
                                      (\e → runInIO (handler e))


--------------------------------------------------------------------------------
--  ** The @handle@ functions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.handle'.
handle ∷ (MonadControlIO m, Exception e) ⇒ (e → m a) → m a → m a
handle handler a = controlIO  $ \runInIO →
                     E.handle (\e → runInIO (handler e))
                              (runInIO a)

-- |Generalized version of 'E.handleJust'.
handleJust ∷ (MonadControlIO m, Exception e)
           ⇒ (e → Maybe b) → (b → m a) → m a → m a
handleJust p handler a = controlIO $ \runInIO →
                           E.handleJust p (\e → runInIO (handler e))
                                          (runInIO a)

sequenceEither ∷ Monad m ⇒ Either e (m a) → m (Either e a)
sequenceEither (Left e)  = return $ Left e
sequenceEither (Right m) = liftM Right m


--------------------------------------------------------------------------------
-- ** The @try@ functions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.try'.
try ∷ (MonadControlIO m, Exception e) ⇒ m a → m (Either e a)
try = liftIOOp_ (liftM sequenceEither ∘ E.try)

-- |Generalized version of 'E.tryJust'.
tryJust ∷ (MonadControlIO m, Exception e) ⇒
           (e → Maybe b) → m a → m (Either b a)
tryJust p = liftIOOp_ (liftM sequenceEither ∘ E.tryJust p)


--------------------------------------------------------------------------------
-- ** The @evaluate@ function
--------------------------------------------------------------------------------

-- |Generalized version of 'E.evaluate'.
evaluate ∷ MonadIO m ⇒ a → m a
evaluate = liftIO ∘ E.evaluate


--------------------------------------------------------------------------------
-- ** Asynchronous exception control
--------------------------------------------------------------------------------

#if MIN_VERSION_base(4,3,0)
-- |Generalized version of 'E.mask'.
mask ∷ MonadControlIO m ⇒ ((∀ a. m a → m a) → m b) → m b
mask f = controlIO $ \runInIO →
           E.mask $ \restore →
             runInIO $ f $ liftIOOp_ restore

-- |Generalized version of 'E.mask_'.
mask_ ∷ MonadControlIO m ⇒ m a → m a
mask_ = liftIOOp_ E.mask_

-- |Generalized version of 'E.uninterruptibleMask'.
uninterruptibleMask ∷ MonadControlIO m ⇒ ((∀ a. m a → m a) → m b) → m b
uninterruptibleMask f = controlIO $ \runInIO →
                          E.uninterruptibleMask $ \restore →
                            runInIO $ f $ liftIOOp_ restore

-- |Generalized version of 'E.uninterruptibleMask_'.
uninterruptibleMask_ ∷ MonadControlIO m ⇒ m a → m a
uninterruptibleMask_ = liftIOOp_ E.uninterruptibleMask_

-- |Generalized version of 'E.getMaskingState'.
getMaskingState ∷ MonadIO m ⇒ m MaskingState
getMaskingState = liftIO E.getMaskingState
#else
-- |Generalized version of 'E.block'.
block ∷ MonadControlIO m ⇒ m a → m a
block = liftIOOp_ E.block

-- |Generalized version of 'E.unblock'.
unblock ∷ MonadControlIO m ⇒ m a → m a
unblock = liftIOOp_ E.unblock
#endif

-- | Generalized version of 'E.blocked'.
-- returns @True@ if asynchronous exceptions are blocked in the
-- current thread.
blocked ∷ MonadIO m ⇒ m Bool
blocked = liftIO E.blocked


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- |Generalized version of 'E.bracket'.  Note, any monadic side
-- effects in @m@ of the \"release\" computation will be discarded; it
-- is run only for its side effects in @IO@.
bracket ∷ MonadControlIO m
        ⇒ m a       -- ^ computation to run first (\"acquire resource\")
        → (a → m b) -- ^ computation to run last (\"release resource\")
        → (a → m c) -- ^ computation to run in-between
        → m c
bracket before after thing = controlIO $ \runInIO →
                               E.bracket (runInIO before)
                                         (\m → runInIO $ m >>= after)
                                         (\m → runInIO $ m >>= thing)

-- |Generalized version of 'E.bracket_'.  Note, any monadic side
-- effects in @m@ of /both/ the \"acquire\" and \"release\"
-- computations will be discarded.  To keep the monadic side effects
-- of the \"acquire\" computation, use 'bracket' with constant
-- functions instead.
bracket_ ∷ MonadControlIO m ⇒ m a → m b → m c → m c
bracket_ before after thing = controlIO $ \runInIO →
                                E.bracket_ (runInIO before)
                                           (runInIO after)
                                           (runInIO thing)

-- |Generalized version of 'E.bracketOnError'.  Note, any monadic side
-- effects in @m@ of the \"release\" computation will be discarded.
bracketOnError ∷ MonadControlIO m
               ⇒ m a       -- ^ computation to run first (\"acquire resource\")
               → (a → m b) -- ^ computation to run last (\"release resource\")
               → (a → m c) -- ^ computation to run in-between
               → m c
bracketOnError before after thing = controlIO $ \runInIO →
                                      E.bracketOnError (runInIO before)
                                                       (\m → runInIO $ m >>= after)
                                                       (\m → runInIO $ m >>= thing)

-- |Generalized version of 'E.finally'.  Note, any monadic side
-- effects in @m@ of the \"afterward\" computation will be discarded.
finally ∷ MonadControlIO m
        ⇒ m a -- ^ computation to run first
        → m b -- ^ computation to run afterward (even if an exception was raised)
        → m a
finally a sequel = controlIO $ \runInIO →
                     E.finally (runInIO a)
                               (runInIO sequel)

-- |Generalized version of 'E.onException'.  Note, any monadic side
-- effects in @m@ of the \"afterward\" computation will be discarded.
onException ∷ MonadControlIO m ⇒ m a → m b → m a
onException m what = controlIO $ \runInIO →
                       E.onException (runInIO m)
                                     (runInIO what)


-- The End ---------------------------------------------------------------------
