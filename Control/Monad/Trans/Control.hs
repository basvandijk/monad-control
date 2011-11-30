{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , RankNTypes
           , TypeFamilies
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
  #-}

{- |
Module      :  Control.Monad.Trans.Control
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

(TODO: It would be nicer if the associated /data types/ 'StT' and 'StM' were
associated /type synonyms/ instead. This would simplify a lot of code and could
make some definitions more efficient because there'll be no need to wrap the
monadic state in a data type. Unfortunately GHC has a bug which prevents this:
<http://hackage.haskell.org/trac/ghc/ticket/5595>. I will switch to associated
type synonyms when that bug is fixed.)
-}

module Control.Monad.Trans.Control
    ( MonadTransControl(..), Run

    , MonadBaseControl(..), RunInBase, ComposeSt, liftBaseControlDefault

    , controlBase

    , liftBaseOp, liftBaseOp_
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( ($) )
import Data.Monoid   ( Monoid, mempty )
import Control.Monad ( Monad, (>>=), return, liftM )

import System.IO                       ( IO )
import GHC.Conc.Sync                   ( STM )
import Data.Maybe                      ( Maybe )
import Data.Either                     ( Either )

import           Control.Monad.ST.Lazy             ( ST )
import qualified Control.Monad.ST.Strict as Strict ( ST )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.Trans.Class    ( MonadTrans )

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

import Data.Functor.Identity ( Identity )

-- from transformers-base:
import Control.Monad.Base ( MonadBase )


--------------------------------------------------------------------------------
-- MonadTransControl type class
--------------------------------------------------------------------------------

class MonadTrans t ⇒ MonadTransControl t where
  -- | Monadic state of @t@.
  data StT t ∷ * → *

  -- | @liftControl@ is similar to 'lift' in that it lifts a computation from
  -- the argument monad to the constructed monad.
  --
  -- Instances should satisfy similar laws as the 'MonadTrans' laws:
  --
  -- @liftControl . const . return = return@
  --
  -- @liftControl (const (m >>= f)) = liftControl (const m) >>= liftControl . const . f@
  --
  -- The difference with 'lift' is that before lifting the @m@ computation
  -- @liftControl@ captures the state of @t@. It then provides the @m@
  -- computation with a 'Run' function that allows running @t n@ computations in
  -- @n@ (for all @n@) on the captured state.
  liftControl ∷ Monad m ⇒ (Run t → m α) → t m α

  -- | Construct a @t@ computation from the monadic state of @t@ that is
  -- returned from a 'Run' function.
  --
  -- Instances should satisfy:
  --
  -- @liftControl (\\run -> run t) >>= restoreT = t@
  restoreT ∷ Monad m ⇒ StT t α → t m α

-- | A function that runs a transformed monad @t n@ on the monadic state that
-- was captured by 'liftControl'
--
-- A @Run t@ function yields a computation in @n@ that returns the monadic state
-- of @t@. This state can later be used to restore a @t@ computation using
-- 'restoreT'.
type Run t = ∀ n β. Monad n ⇒ t n β → n (StT t β)


--------------------------------------------------------------------------------
-- MonadTransControl instances
--------------------------------------------------------------------------------

instance MonadTransControl IdentityT where
    newtype StT IdentityT α = StId α
    liftControl f = IdentityT $ f $ liftM StId ∘ runIdentityT
    restoreT (StId x) = return x
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance MonadTransControl MaybeT where
    newtype StT MaybeT α = StMaybe (Maybe α)
    liftControl f = MaybeT $ liftM return $ f $ liftM StMaybe ∘ runMaybeT
    restoreT (StMaybe mb) = MaybeT $ return mb
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance Error e ⇒ MonadTransControl (ErrorT e) where
    newtype StT (ErrorT e) α = StError (Either e α)
    liftControl f = ErrorT $ liftM return $ f $ liftM StError ∘ runErrorT
    restoreT (StError e) = ErrorT $ return e
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance MonadTransControl ListT where
    newtype StT ListT α = StList [α]
    liftControl f = ListT $ liftM return $ f $ liftM StList ∘ runListT
    restoreT (StList l) = ListT $ return l
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance MonadTransControl (ReaderT r) where
    newtype StT (ReaderT r) α = StReader α
    liftControl f = ReaderT $ \r → f $ \t → liftM StReader $ runReaderT t r
    restoreT (StReader x) = return x
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance MonadTransControl (StateT s) where
    newtype StT (StateT s) α = StState (α, s)
    liftControl f = StateT $ \s →
                      liftM (\x → (x, s))
                            (f $ \t → liftM StState $ runStateT t s)
    restoreT (StState st) = StateT $ \_ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance MonadTransControl (Strict.StateT s) where
    newtype StT (Strict.StateT s) α = StState' (α, s)
    liftControl f = Strict.StateT $ \s →
                      liftM (\x → (x, s))
                            (f $ \t → liftM StState' $ Strict.runStateT t s)
    restoreT (StState' st) = Strict.StateT $ \_ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (WriterT w) where
    newtype StT (WriterT w) α = StWriter (α, w)
    liftControl f = WriterT $ liftM (\x → (x, mempty))
                                    (f $ liftM StWriter ∘ runWriterT)
    restoreT (StWriter st) = WriterT $ return st
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (Strict.WriterT w) where
    newtype StT (Strict.WriterT w) α = StWriter' (α, w)
    liftControl f = Strict.WriterT $ liftM (\x → (x, mempty))
                                           (f $ liftM StWriter' ∘ Strict.runWriterT)
    restoreT (StWriter' st) = Strict.WriterT $ return st
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (RWST r w s) where
    newtype StT (RWST r w s) α = StRWS (α, s, w)
    liftControl f =
        RWST $ \r s → liftM (\x → (x, s, mempty))
                            (f $ \t → liftM StRWS $ runRWST t r s)
    restoreT (StRWS st) = RWST $ \_ _ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (Strict.RWST r w s) where
    newtype StT (Strict.RWST r w s) α = StRWS' (α, s, w)
    liftControl f =
        Strict.RWST $ \r s → liftM (\x → (x, s, mempty))
                                   (f $ \t → liftM StRWS' $ Strict.runRWST t r s)
    restoreT (StRWS' st) = Strict.RWST $ \_ _ → return st
    {-# INLINE liftControl #-}
    {-# INLINE restoreT #-}


--------------------------------------------------------------------------------
-- MonadBaseControl type class
--------------------------------------------------------------------------------

class MonadBase b m ⇒ MonadBaseControl b m | m → b where
    -- | Monadic state of @m@.
    --
    -- Note for instance writers: This can just be a newtype wrapper around
    -- 'ComposeSt'. For example when writing a @MonadBaseControl@ instance for
    -- your monad transformer @T@ you can use the following:
    --
    -- @newtype StM (T m) a = StMT ('ComposeSt' T m a)@
    data StM m ∷ * → *

    -- | @liftBaseControl@ is similar to 'liftIO' and 'liftBase' in that it
    -- lifts a base computation to the constructed monad.
    --
    -- Instances should satisfy similar laws as the 'MonadIO' and 'MonadBase' laws:
    --
    -- @liftBaseControl . const . return = return@
    --
    -- @liftBaseControl (const (m >>= f)) = liftBaseControl (const m) >>= liftBaseControl . const . f@
    --
    -- The difference with 'liftBase' is that before lifting the base computation
    -- @liftBaseControl@ captures the state of @m@. It then provides the base
    -- computation with a 'RunInBase' function that allows running @m@
    -- computations in the base monad on the captured state.
    --
    -- Note for instance writers: you can use 'liftBaseControlDefault' to define
    -- this method (This assumes that your 'StM' is defined using 'ComposeSt'
    -- as discussed above):
    --
    -- @liftBaseControl = 'liftBaseControlDefault' StMT@
    liftBaseControl ∷ (RunInBase m b → b α) → m α

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a 'RunInBase' function.
    --
    -- Instances should satisfy:
    --
    -- @liftBaseControl (\\runInBase -> runInBase m) >>= restoreM = m@
    restoreM ∷ StM m α → m α

-- | A function that runs a @m@ computation on the monadic state that was
-- captured by 'liftBaseControl'
--
-- A @RunInBase m@ function yields a computation in the base monad of @m@ that
-- returns the monadic state of @m@. This state can later be used to restore the
-- @m@ computation using 'restoreM'.
type RunInBase m b = ∀ α. m α → b (StM m α)

-- | An often used composition: @controlBase f = 'liftBaseControl' f >>= 'restoreM'@
controlBase ∷ MonadBaseControl b m ⇒ (RunInBase m b → b (StM m α)) → m α
controlBase f = liftBaseControl f >>= restoreM
{-# INLINE controlBase #-}


--------------------------------------------------------------------------------
-- MonadBaseControl instances for all monads in the base library
--------------------------------------------------------------------------------

#define BASE(M, ST)                       \
instance MonadBaseControl (M) (M) where { \
    newtype StM (M) α = ST α;             \
    liftBaseControl f = f $ liftM ST;     \
    restoreM (ST x) = return x;           \
    {-# INLINE liftBaseControl #-};       \
    {-# INLINE restoreM #-}}

BASE(IO,          StIO)
BASE(ST s,        StST)
BASE(Strict.ST s, StSTS)
BASE(STM,         StSTM)
BASE(Maybe,       St)
BASE(Either e,    StE)
BASE([],          StL)
BASE((→) r,       StF)
BASE(Identity,    StI)
#undef BASE


--------------------------------------------------------------------------------
-- liftBaseControlDefault
--------------------------------------------------------------------------------

-- | Handy type synonym that composes the monadic states of @t@ and @m@.
--
-- It can be used to define the 'StIO' for new 'MonadBaseControl' instances.
type ComposeSt t m α = StM m (StT t α)

-- | Can be used to give a defintion of 'liftBaseControl'.
--
-- It basically composes a 'liftControl' of @t@ with a 'liftBaseControl' of @m@ to
-- give a 'liftBaseControl' of @t m@.
liftBaseControlDefault ∷ ( MonadTransControl t
                         , MonadBaseControl b m
                         , Monad (t m)
                         , Monad m
                         )
                       ⇒ (∀ β. ComposeSt t m β → StM (t m) β) -- ^ 'StM' constructor
                       → ((RunInBase (t m) b  → b α) → t m α)
liftBaseControlDefault stBase = \f → liftControl $ \run →
                                       liftBaseControl $ \runInBase →
                                         f $ liftM stBase ∘ runInBase ∘ run
{-# INLINE liftBaseControlDefault #-}


--------------------------------------------------------------------------------
-- MonadBaseControl transformer instances
--------------------------------------------------------------------------------

instance MonadBaseControl b m ⇒ MonadBaseControl b (IdentityT m) where
    newtype StM (IdentityT m) α = StMId (ComposeSt IdentityT m α)
    liftBaseControl = liftBaseControlDefault StMId
    restoreM (StMId stBase) = IdentityT $ restoreM stBase >>= runIdentityT ∘ restoreT
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance MonadBaseControl b m ⇒ MonadBaseControl b (ListT m) where
    newtype StM (ListT m) α = StMList (ComposeSt ListT m α)
    liftBaseControl = liftBaseControlDefault StMList
    restoreM (StMList stBase) = ListT $ restoreM stBase >>= runListT ∘ restoreT
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance MonadBaseControl b m ⇒ MonadBaseControl b (MaybeT m) where
    newtype StM (MaybeT m) α = StMMaybe (ComposeSt MaybeT m α)
    liftBaseControl = liftBaseControlDefault StMMaybe
    restoreM (StMMaybe stBase) = MaybeT $ restoreM stBase >>= runMaybeT ∘ restoreT
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance (Error e, MonadBaseControl b m) ⇒ MonadBaseControl b (ErrorT e m) where
    newtype StM (ErrorT e m) α = StMError (ComposeSt (ErrorT e) m α)
    liftBaseControl = liftBaseControlDefault StMError
    restoreM (StMError stBase) = ErrorT $ restoreM stBase >>= runErrorT ∘ restoreT
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance MonadBaseControl b m ⇒ MonadBaseControl b (ReaderT r m) where
    newtype StM (ReaderT r m) α = StMReader (ComposeSt (ReaderT r) m α)
    liftBaseControl = liftBaseControlDefault StMReader
    restoreM (StMReader stBase) = ReaderT $ \r → do
                                   st ← restoreM stBase
                                   runReaderT (restoreT st) r
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance MonadBaseControl b m ⇒ MonadBaseControl b (StateT s m) where
    newtype StM (StateT s m) α = StMState (ComposeSt (StateT s) m α)
    liftBaseControl = liftBaseControlDefault StMState
    restoreM (StMState stBase) = StateT $ \s → do
                                  st ← restoreM stBase
                                  runStateT (restoreT st) s
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance MonadBaseControl b m ⇒ MonadBaseControl b (Strict.StateT s m) where
    newtype StM (Strict.StateT s m) α = StMState' (ComposeSt (Strict.StateT s) m α)
    liftBaseControl = liftBaseControlDefault StMState'
    restoreM (StMState' stBase) = Strict.StateT $ \s → do
                                   st ← restoreM stBase
                                   Strict.runStateT (restoreT st) s
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance (Monoid w, MonadBaseControl b m) ⇒ MonadBaseControl b (WriterT w m) where
    newtype StM (WriterT w m) α = StMWriter (ComposeSt (WriterT w) m α)
    liftBaseControl =liftBaseControlDefault StMWriter
    restoreM (StMWriter stBase) = WriterT $ do
                                   st ← restoreM stBase
                                   runWriterT (restoreT st)
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance (Monoid w, MonadBaseControl b m) ⇒ MonadBaseControl b (Strict.WriterT w m) where
    newtype StM (Strict.WriterT w m) α = StMWriter' (ComposeSt (Strict.WriterT w) m α)
    liftBaseControl = liftBaseControlDefault StMWriter'
    restoreM (StMWriter' stBase) = Strict.WriterT $ do
                                    st ← restoreM stBase
                                    Strict.runWriterT (restoreT st)
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance (Monoid w, MonadBaseControl b m) ⇒ MonadBaseControl b (RWST r w s m) where
    newtype StM (RWST r w s m) α = StMRWS (ComposeSt (RWST r w s) m α)
    liftBaseControl = liftBaseControlDefault StMRWS
    restoreM (StMRWS stBase) = RWST $ \r s → do
                                st ← restoreM stBase
                                runRWST (restoreT st) r s
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}

instance (Monoid w, MonadBaseControl b m) ⇒ MonadBaseControl b (Strict.RWST r w s m) where
    newtype StM (Strict.RWST r w s m) α = StMRWS' (ComposeSt (Strict.RWST r w s) m α)
    liftBaseControl = liftBaseControlDefault StMRWS'
    restoreM (StMRWS' stBase) = Strict.RWST $ \r s → do
                                 st ← restoreM stBase
                                 Strict.runRWST (restoreT st) r s
    {-# INLINE liftBaseControl #-}
    {-# INLINE restoreM #-}


--------------------------------------------------------------------------------
-- Convenient lifting of two common special cases of control operation types
--------------------------------------------------------------------------------

{-|
@liftBaseOp@ is a particular application of 'liftBaseControl' that allows
lifting control operations of type:

@((a -> 'IO' b) -> 'IO' b)@      (e.g. @alloca@, @withMVar v@) to:

@('MonadBaseControl' m 'IO' => (a -> b m) -> b m)@.
-}
liftBaseOp ∷ MonadBaseControl b m
           ⇒ ((α → b (StM m β)) → b (StM m γ))
           → ((α →        m β)  →        m γ)
liftBaseOp f = \g → controlBase $ \runInBase → f $ runInBase ∘ g
{-# INLINE liftBaseOp #-}

{-|
@liftBaseOp_@ is a particular application of 'liftBaseControl' that allows
lifting control operations of type:

@('IO' a -> 'IO' a)@  (e.g. @mask_@) to:

@('MonadBaseControl' m => m a -> m a)@.
-}
liftBaseOp_ ∷ MonadBaseControl b m
            ⇒ (b (StM m α) → b (StM m β))
            → (       m α  →        m β)
liftBaseOp_ f = \m → controlBase $ \runInBase → f $ runInBase m
{-# INLINE liftBaseOp_ #-}
