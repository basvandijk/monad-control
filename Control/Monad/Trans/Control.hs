{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , RankNTypes
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , TypeFamilies

           , ScopedTypeVariables
  #-}

{- |
Module      :  Control.Monad.Trans.Control
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
-}

module Control.Monad.Trans.Control
    ( MonadTransControl(..), Run

    , MonadControlIO(..), RunInIO, AllSt, liftControlIODefault

    , controlIO

    , liftIOOp, liftIOOp_
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Maybe    ( Maybe )
import Data.Either   ( Either )
import Data.Function ( ($) )
import Data.Monoid   ( Monoid, mempty )
import Control.Monad ( Monad, (>>=), return, liftM )
import System.IO     ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.Trans.Class    ( MonadTrans )
import Control.Monad.IO.Class       ( MonadIO )

import Control.Monad.Trans.Identity ( IdentityT(IdentityT), runIdentityT )
import Control.Monad.Trans.List     ( ListT    (ListT),     runListT )
import Control.Monad.Trans.Maybe    ( MaybeT   (MaybeT),    runMaybeT )
import Control.Monad.Trans.Error    ( ErrorT   (ErrorT),    runErrorT, Error )
import Control.Monad.Trans.Reader   ( ReaderT  (ReaderT),   runReaderT )
import Control.Monad.Trans.State    ( StateT   (StateT),    runStateT )
import Control.Monad.Trans.Writer   ( WriterT  (WriterT),   runWriterT )
import Control.Monad.Trans.RWS      ( RWST     (RWST),      runRWST )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   (RWST),    runRWST )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT (StateT),  runStateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT(WriterT), runWriterT )


--------------------------------------------------------------------------------
-- MonadTransControl type class
--------------------------------------------------------------------------------

{-|
Instances should satisfy similar laws as the 'MonadTrans' laws:

@liftControl . const . return = return@

@liftControl (const (m >>= f)) = liftControl (const m) >>= liftControl . const . f@

Additionally instances should satisfy:

@liftControl (\\run -> run t) >>= restore = t@
-}
class MonadTrans t ⇒ MonadTransControl t where
  data St t ∷ * → *

  liftControl ∷ Monad m ⇒ (Run t → m α) → t m α

  restore ∷ Monad m ⇒ St t α → t m α

type Run t = ∀ n β. Monad n ⇒ t n β → n (St t β)


--------------------------------------------------------------------------------
-- MonadTransControl instances
--------------------------------------------------------------------------------

instance MonadTransControl IdentityT where
    newtype St IdentityT α = StId α
    liftControl f = IdentityT $ f $ liftM StId ∘ runIdentityT
    restore (StId x) = return x
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance MonadTransControl MaybeT where
    newtype St MaybeT α = StMaybe (Maybe α)
    liftControl f = MaybeT $ liftM return $ f $ liftM StMaybe ∘ runMaybeT
    restore (StMaybe mb) = MaybeT $ return mb
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance Error e ⇒ MonadTransControl (ErrorT e) where
    newtype St (ErrorT e) α = StError (Either e α)
    liftControl f = ErrorT $ liftM return $ f $ liftM StError ∘ runErrorT
    restore (StError e) = ErrorT $ return e
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance MonadTransControl ListT where
    newtype St ListT α = StList [α]
    liftControl f = ListT $ liftM return $ f $ liftM StList ∘ runListT
    restore (StList l) = ListT $ return l
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance MonadTransControl (ReaderT r) where
    newtype St (ReaderT r) α = StReader α
    liftControl f = ReaderT $ \r → f $ \t → liftM StReader $ runReaderT t r
    restore (StReader x) = return x
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance MonadTransControl (StateT s) where
    newtype St (StateT s) α = StState (α, s)
    liftControl f = StateT $ \s →
                      liftM (\x → (x, s))
                            (f $ \t → liftM StState $ runStateT t s)
    restore (StState st) = StateT $ \_ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance MonadTransControl (Strict.StateT s) where
    newtype St (Strict.StateT s) α = StState' (α, s)
    liftControl f = Strict.StateT $ \s →
                      liftM (\x → (x, s))
                            (f $ \t → liftM StState' $ Strict.runStateT t s)
    restore (StState' st) = Strict.StateT $ \_ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance Monoid w ⇒ MonadTransControl (WriterT w) where
    newtype St (WriterT w) α = StWriter (α, w)
    liftControl f = WriterT $ liftM (\x → (x, mempty))
                                    (f $ liftM StWriter ∘ runWriterT)
    restore (StWriter st) = WriterT $ return st
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance Monoid w ⇒ MonadTransControl (Strict.WriterT w) where
    newtype St (Strict.WriterT w) α = StWriter' (α, w)
    liftControl f = Strict.WriterT $ liftM (\x → (x, mempty))
                                           (f $ liftM StWriter' ∘ Strict.runWriterT)
    restore (StWriter' st) = Strict.WriterT $ return st
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance Monoid w ⇒ MonadTransControl (RWST r w s) where
    newtype St (RWST r w s) α = StRWS (α, s, w)
    liftControl f =
        RWST $ \r s → liftM (\x → (x, s, mempty))
                            (f $ \t → liftM StRWS $ runRWST t r s)
    restore (StRWS st) = RWST $ \_ _ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}

instance Monoid w ⇒ MonadTransControl (Strict.RWST r w s) where
    newtype St (Strict.RWST r w s) α = StRWS' (α, s, w)
    liftControl f =
        Strict.RWST $ \r s → liftM (\x → (x, s, mempty))
                                   (f $ \t → liftM StRWS' $ Strict.runRWST t r s)
    restore (StRWS' st) = Strict.RWST $ \_ _ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restore #-}


--------------------------------------------------------------------------------
-- MonadControlIO type class
--------------------------------------------------------------------------------

{-|
Instances should satisfy similar laws as the 'MonadIO' laws:

@liftControlIO . const . return = return@

@liftControlIO (const (m >>= f)) = liftControlIO (const m) >>= liftControlIO . const . f@

Additionally instances should satisfy:

@liftControlIO (\\runInIO -> runInIO m) >>= restoreIO = m@
-}
class MonadIO m ⇒ MonadControlIO m where
    data StIO m ∷ * → *

    liftControlIO ∷ (RunInIO m → IO α) → m α

    restoreIO ∷ StIO m α → m α

type RunInIO m = ∀ β. m β → IO (StIO m β)

-- | An often used composition: @controlIO f = 'liftControlIO' f >>= 'restoreIO'@
controlIO ∷ MonadControlIO m ⇒ (RunInIO m → IO (StIO m α)) → m α
controlIO f = liftControlIO f >>= restoreIO
{-# INLINE controlIO #-}

instance MonadControlIO IO where
    newtype StIO IO α = StIO α
    liftControlIO f = f $ liftM StIO
    restoreIO (StIO x) = return x
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

type AllSt t m α = StIO m (St t α)

liftControlIODefault ∷ (MonadTransControl t, MonadControlIO m, Monad (t m), Monad m)
                     ⇒ (∀ β. AllSt t m β → StIO (t m) β) -- ^ 'StIO' constructor
                     → ((RunInIO (t m) → IO α) → t m α)
liftControlIODefault stIO = \f → liftControl $ \run →
                                   liftControlIO $ \runInIO →
                                     f $ liftM stIO ∘ runInIO ∘ run
{-# INLINE liftControlIODefault #-}

instance MonadControlIO m ⇒ MonadControlIO (IdentityT m) where
    newtype StIO (IdentityT m) α = StIOId (AllSt IdentityT m α)
    liftControlIO = liftControlIODefault StIOId
    restoreIO (StIOId stIO) = IdentityT $ restoreIO stIO >>= runIdentityT ∘ restore
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance MonadControlIO m ⇒ MonadControlIO (ListT m) where
    newtype StIO (ListT m) α = StIOList (AllSt ListT m α)
    liftControlIO = liftControlIODefault StIOList
    restoreIO (StIOList stIO) = ListT $ restoreIO stIO >>= runListT ∘ restore
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance MonadControlIO m ⇒ MonadControlIO (MaybeT m) where
    newtype StIO (MaybeT m) α = StIOMaybe (AllSt MaybeT m α)
    liftControlIO = liftControlIODefault StIOMaybe
    restoreIO (StIOMaybe stIO) = MaybeT $ restoreIO stIO >>= runMaybeT ∘ restore
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance (Error e, MonadControlIO m) ⇒ MonadControlIO (ErrorT e m) where
    newtype StIO (ErrorT e m) α = StIOError (AllSt (ErrorT e) m α)
    liftControlIO = liftControlIODefault StIOError
    restoreIO (StIOError stIO) = ErrorT $ restoreIO stIO >>= runErrorT ∘ restore
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance MonadControlIO m ⇒ MonadControlIO (ReaderT r m) where
    newtype StIO (ReaderT r m) α = StIOReader (AllSt (ReaderT r) m α)
    liftControlIO = liftControlIODefault StIOReader
    restoreIO (StIOReader stIO) = ReaderT $ \r → do
                                    st ← restoreIO stIO
                                    runReaderT (restore st) r
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance MonadControlIO m ⇒ MonadControlIO (StateT s m) where
    newtype StIO (StateT s m) α = StIOState (AllSt (StateT s) m α)
    liftControlIO = liftControlIODefault StIOState
    restoreIO (StIOState stIO) = StateT $ \s → do
                                   st ← restoreIO stIO
                                   runStateT (restore st) s
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance MonadControlIO m ⇒ MonadControlIO (Strict.StateT s m) where
    newtype StIO (Strict.StateT s m) α = StIOState' (AllSt (Strict.StateT s) m α)
    liftControlIO = liftControlIODefault StIOState'
    restoreIO (StIOState' stIO) = Strict.StateT $ \s → do
                                    st ← restoreIO stIO
                                    Strict.runStateT (restore st) s
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (WriterT w m) where
    newtype StIO (WriterT w m) α = StIOWriter (AllSt (WriterT w) m α)
    liftControlIO =liftControlIODefault StIOWriter
    restoreIO (StIOWriter stIO) = WriterT $ do
                                    st ← restoreIO stIO
                                    runWriterT (restore st)
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (Strict.WriterT w m) where
    newtype StIO (Strict.WriterT w m) α = StIOWriter' (AllSt (Strict.WriterT w) m α)
    liftControlIO = liftControlIODefault StIOWriter'
    restoreIO (StIOWriter' stIO) = Strict.WriterT $ do
                                     st ← restoreIO stIO
                                     Strict.runWriterT (restore st)
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (RWST r w s m) where
    newtype StIO (RWST r w s m) α = StIORWS (AllSt (RWST r w s) m α)
    liftControlIO = liftControlIODefault StIORWS
    restoreIO (StIORWS stIO) = RWST $ \r s → do
                                 st ← restoreIO stIO
                                 runRWST (restore st) r s
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (Strict.RWST r w s m) where
    newtype StIO (Strict.RWST r w s m) α = StIORWS' (AllSt (Strict.RWST r w s) m α)
    liftControlIO = liftControlIODefault StIORWS'
    restoreIO (StIORWS' stIO) = Strict.RWST $ \r s → do
                                  st ← restoreIO stIO
                                  Strict.runRWST (restore st) r s
    {-# INLINE liftControlIO #-}
    {-# INLINE restoreIO #-}


--------------------------------------------------------------------------------
-- Convenient lifting of two common special cases of control operation types
--------------------------------------------------------------------------------

{-|
@liftIOOp@ is a particular application of 'liftControlIO' that allows
lifting control operations of type:

@((a -> 'IO' b) -> 'IO' b)@      (e.g. @alloca@, @withMVar v@) to:

@('MonadControlIO' m => (a -> m b) -> m b)@.
-}
liftIOOp ∷ MonadControlIO m
         ⇒ ((α → IO (StIO m β)) → IO (StIO m γ))
         → ((α →          m β)  →          m γ)
liftIOOp f = \g → controlIO $ \runInIO → f $ runInIO ∘ g
{-# INLINE liftIOOp #-}

{-|
@liftIOOp_@ is a particular application of 'liftControlIO' that allows
lifting control operations of type:

@('IO' a -> 'IO' a)@  (e.g. @mask_@) to:

@('MonadControlIO' m => m a -> m a)@.
-}
liftIOOp_ ∷ MonadControlIO m
          ⇒ (IO (StIO m α) → IO (StIO m β))
          → (         m α  →          m β)
liftIOOp_ f = \m → controlIO $ \runInIO → f $ runInIO m
{-# INLINE liftIOOp_ #-}
