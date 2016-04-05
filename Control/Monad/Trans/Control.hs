{-# LANGUAGE CPP
           , NoImplicitPrelude
           , RankNTypes
           , TypeFamilies
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

#if MIN_VERSION_transformers(0,4,0)
-- Hide warnings for the deprecated ErrorT transformer:
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

{- |
Module      :  Control.Monad.Trans.Control
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
-}

module Control.Monad.Trans.Control
    ( -- * MonadTransControl
      MonadTransControl(..), Run

      -- ** Defaults
      -- $MonadTransControlDefaults
    , RunDefault, defaultLiftWith, defaultRestoreT

      -- * MonadBaseControl
    , MonadBaseControl (..), RunInBase

      -- ** Defaults
      -- $MonadBaseControlDefaults
    , ComposeSt, RunInBaseDefault, defaultLiftBaseWith, defaultRestoreM

      -- * Utility functions
    , control, embed, embed_, captureT, captureM

    , liftBaseOp, liftBaseOp_

    , liftBaseDiscard, liftBaseOpDiscard

    , liftThrough
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( (.), ($), const )
import Data.Monoid   ( Monoid, mempty )
import Control.Monad ( Monad, (>>=), return, liftM )
import System.IO     ( IO )
import Data.Maybe    ( Maybe )
import Data.Either   ( Either )

#if MIN_VERSION_base(4,4,0)
import           Control.Monad.ST.Lazy.Safe           ( ST )
import qualified Control.Monad.ST.Safe      as Strict ( ST )
#endif

-- from stm:
import Control.Monad.STM ( STM )

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
import Control.Monad.Trans.Except   ( ExceptT  (ExceptT),   runExceptT )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   (RWST),    runRWST )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT (StateT),  runStateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT(WriterT), runWriterT )

import Data.Functor.Identity ( Identity )

-- from transformers-base:
import Control.Monad.Base ( MonadBase )

#if MIN_VERSION_base(4,3,0)
import Control.Monad ( void )
#else
import Data.Functor (Functor, fmap)
void :: Functor f => f a -> f ()
void = fmap (const ())
#endif

import Prelude (id)

--------------------------------------------------------------------------------
-- MonadTransControl type class
--------------------------------------------------------------------------------

class MonadTrans t => MonadTransControl t where
  -- | Monadic state of @t@.
  type StT t a :: *

  -- | @liftWith@ is similar to 'lift' in that it lifts a computation from
  -- the argument monad to the constructed monad.
  --
  -- Instances should satisfy similar laws as the 'MonadTrans' laws:
  --
  -- @liftWith . const . return = return@
  --
  -- @liftWith (const (m >>= f)) = liftWith (const m) >>= liftWith . const . f@
  --
  -- The difference with 'lift' is that before lifting the @m@ computation
  -- @liftWith@ captures the state of @t@. It then provides the @m@
  -- computation with a 'Run' function that allows running @t n@ computations in
  -- @n@ (for all @n@) on the captured state.
  liftWith :: Monad m => (Run t -> m a) -> t m a

  -- | Construct a @t@ computation from the monadic state of @t@ that is
  -- returned from a 'Run' function.
  --
  -- Instances should satisfy:
  --
  -- @liftWith (\\run -> run t) >>= restoreT . return = t@
  restoreT :: Monad m => m (StT t a) -> t m a

-- | A function that runs a transformed monad @t n@ on the monadic state that
-- was captured by 'liftWith'
--
-- A @Run t@ function yields a computation in @n@ that returns the monadic state
-- of @t@. This state can later be used to restore a @t@ computation using
-- 'restoreT'.
type Run t = forall n b. Monad n => t n b -> n (StT t b)


--------------------------------------------------------------------------------
-- Defaults for MonadTransControl
--------------------------------------------------------------------------------

-- $MonadTransControlDefaults
--
-- The following functions can be used to define a 'MonadTransControl' instance
-- for a monad transformer which simply wraps another monad transformer which
-- already has a @MonadTransControl@ instance. For example:
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
--
-- newtype CounterT m a = CounterT {unCounterT :: StateT Int m a}
--   deriving (Monad, MonadTrans)
--
-- instance MonadTransControl CounterT where
--     type StT CounterT a = StT (StateT Int) a
--     liftWith = 'defaultLiftWith' CounterT unCounterT
--     restoreT = 'defaultRestoreT' CounterT
-- @

-- | A function like 'Run' that runs a monad transformer @t@ which wraps the
-- monad transformer @t'@. This is used in 'defaultLiftWith'.
type RunDefault t t' = forall n b. Monad n => t n b -> n (StT t' b)

-- | Default definition for the 'liftWith' method.
defaultLiftWith :: (Monad m, MonadTransControl n)
                => (forall b.   n m b -> t m b)     -- ^ Monad constructor
                -> (forall o b. t o b -> n o b)     -- ^ Monad deconstructor
                -> (RunDefault t n -> m a)
                -> t m a
defaultLiftWith t unT = \f -> t $ liftWith $ \run -> f $ run . unT
{-# INLINABLE defaultLiftWith #-}

-- | Default definition for the 'restoreT' method.
defaultRestoreT :: (Monad m, MonadTransControl n)
                => (n m a -> t m a)     -- ^ Monad constructor
                -> m (StT n a)
                -> t m a
defaultRestoreT t = t . restoreT
{-# INLINABLE defaultRestoreT #-}


--------------------------------------------------------------------------------
-- MonadTransControl instances
--------------------------------------------------------------------------------

instance MonadTransControl IdentityT where
    type StT IdentityT a = a
    liftWith f = IdentityT $ f $ runIdentityT
    restoreT = IdentityT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl MaybeT where
    type StT MaybeT a = Maybe a
    liftWith f = MaybeT $ liftM return $ f $ runMaybeT
    restoreT = MaybeT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Error e => MonadTransControl (ErrorT e) where
    type StT (ErrorT e) a = Either e a
    liftWith f = ErrorT $ liftM return $ f $ runErrorT
    restoreT = ErrorT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (ExceptT e) where
    type StT (ExceptT e) a = Either e a
    liftWith f = ExceptT $ liftM return $ f $ runExceptT
    restoreT = ExceptT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl ListT where
    type StT ListT a = [a]
    liftWith f = ListT $ liftM return $ f $ runListT
    restoreT = ListT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (ReaderT r) where
    type StT (ReaderT r) a = a
    liftWith f = ReaderT $ \r -> f $ \t -> runReaderT t r
    restoreT = ReaderT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (StateT s) where
    type StT (StateT s) a = (a, s)
    liftWith f = StateT $ \s ->
                   liftM (\x -> (x, s))
                         (f $ \t -> runStateT t s)
    restoreT = StateT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (Strict.StateT s) where
    type StT (Strict.StateT s) a = (a, s)
    liftWith f = Strict.StateT $ \s ->
                   liftM (\x -> (x, s))
                         (f $ \t -> Strict.runStateT t s)
    restoreT = Strict.StateT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (WriterT w) where
    type StT (WriterT w) a = (a, w)
    liftWith f = WriterT $ liftM (\x -> (x, mempty))
                                 (f $ runWriterT)
    restoreT = WriterT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (Strict.WriterT w) where
    type StT (Strict.WriterT w) a = (a, w)
    liftWith f = Strict.WriterT $ liftM (\x -> (x, mempty))
                                        (f $ Strict.runWriterT)
    restoreT = Strict.WriterT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (RWST r w s) where
    type StT (RWST r w s) a = (a, s, w)
    liftWith f = RWST $ \r s -> liftM (\x -> (x, s, mempty))
                                      (f $ \t -> runRWST t r s)
    restoreT mSt = RWST $ \_ _ -> mSt
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (Strict.RWST r w s) where
    type StT (Strict.RWST r w s) a = (a, s, w)
    liftWith f =
        Strict.RWST $ \r s -> liftM (\x -> (x, s, mempty))
                                    (f $ \t -> Strict.runRWST t r s)
    restoreT mSt = Strict.RWST $ \_ _ -> mSt
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}


--------------------------------------------------------------------------------
-- MonadBaseControl type class
--------------------------------------------------------------------------------

class MonadBase b m => MonadBaseControl b m | m -> b where
    -- | Monadic state of @m@.
    type StM m a :: *

    -- | @liftBaseWith@ is similar to 'liftIO' and 'liftBase' in that it
    -- lifts a base computation to the constructed monad.
    --
    -- Instances should satisfy similar laws as the 'MonadIO' and 'MonadBase' laws:
    --
    -- @liftBaseWith . const . return = return@
    --
    -- @liftBaseWith (const (m >>= f)) = liftBaseWith (const m) >>= liftBaseWith . const . f@
    --
    -- The difference with 'liftBase' is that before lifting the base computation
    -- @liftBaseWith@ captures the state of @m@. It then provides the base
    -- computation with a 'RunInBase' function that allows running @m@
    -- computations in the base monad on the captured state.
    liftBaseWith :: (RunInBase m b -> b a) -> m a

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a 'RunInBase' function.
    --
    -- Instances should satisfy:
    --
    -- @liftBaseWith (\\runInBase -> runInBase m) >>= restoreM = m@
    restoreM :: StM m a -> m a

-- | A function that runs a @m@ computation on the monadic state that was
-- captured by 'liftBaseWith'
--
-- A @RunInBase m@ function yields a computation in the base monad of @m@ that
-- returns the monadic state of @m@. This state can later be used to restore the
-- @m@ computation using 'restoreM'.
type RunInBase m b = forall a. m a -> b (StM m a)


--------------------------------------------------------------------------------
-- MonadBaseControl instances for all monads in the base library
--------------------------------------------------------------------------------

#define BASE(M)                           \
instance MonadBaseControl (M) (M) where { \
    type StM (M) a = a;                   \
    liftBaseWith f = f id;                \
    restoreM = return;                    \
    {-# INLINABLE liftBaseWith #-};       \
    {-# INLINABLE restoreM #-}}

BASE(IO)
BASE(Maybe)
BASE(Either e)
BASE([])
BASE((->) r)
BASE(Identity)

BASE(STM)

#if MIN_VERSION_base(4,4,0)
BASE(Strict.ST s)
BASE(       ST s)
#endif

#undef BASE


--------------------------------------------------------------------------------
-- Defaults for MonadBaseControl
--------------------------------------------------------------------------------

-- $MonadBaseControlDefaults
--
-- Note that by using the following default definitions it's easy to make a
-- monad transformer @T@ an instance of 'MonadBaseControl':
--
-- @
-- instance MonadBaseControl b m => MonadBaseControl b (T m) where
--     type StM (T m) a = 'ComposeSt' T m a
--     liftBaseWith     = 'defaultLiftBaseWith'
--     restoreM         = 'defaultRestoreM'
-- @
--
-- Defining an instance for a base monad @B@ is equally straightforward:
--
-- @
-- instance MonadBaseControl B B where
--     type StM B a   = a
--     liftBaseWith f = f 'id'
--     restoreM       = 'return'
-- @

-- | Handy type synonym that composes the monadic states of @t@ and @m@.
--
-- It can be used to define the 'StM' for new 'MonadBaseControl' instances.
type ComposeSt t m a = StM m (StT t a)

-- | A function like 'RunInBase' that runs a monad transformer @t@ in its base
-- monad @b@. It is used in 'defaultLiftBaseWith'.
type RunInBaseDefault t m b = forall a. t m a -> b (ComposeSt t m a)

-- | Default defintion for the 'liftBaseWith' method.
--
-- Note that it composes a 'liftWith' of @t@ with a 'liftBaseWith' of @m@ to
-- give a 'liftBaseWith' of @t m@:
--
-- @
-- defaultLiftBaseWith = \\f -> 'liftWith' $ \\run ->
--                               'liftBaseWith' $ \\runInBase ->
--                                 f $ runInBase . run
-- @
defaultLiftBaseWith :: (MonadTransControl t, MonadBaseControl b m)
                    => (RunInBaseDefault t m b -> b a) -> t m a
defaultLiftBaseWith = \f -> liftWith $ \run ->
                              liftBaseWith $ \runInBase ->
                                f $ runInBase . run
{-# INLINABLE defaultLiftBaseWith #-}

-- | Default definition for the 'restoreM' method.
--
-- Note that: @defaultRestoreM = 'restoreT' . 'restoreM'@
defaultRestoreM :: (MonadTransControl t, MonadBaseControl b m)
                => ComposeSt t m a -> t m a
defaultRestoreM = restoreT . restoreM
{-# INLINABLE defaultRestoreM #-}


--------------------------------------------------------------------------------
-- MonadBaseControl transformer instances
--------------------------------------------------------------------------------

#define BODY(T) {                         \
    type StM (T m) a = ComposeSt (T) m a; \
    liftBaseWith = defaultLiftBaseWith;   \
    restoreM     = defaultRestoreM;       \
    {-# INLINABLE liftBaseWith #-};       \
    {-# INLINABLE restoreM #-}}

#define TRANS(         T) \
  instance (     MonadBaseControl b m) => MonadBaseControl b (T m) where BODY(T)
#define TRANS_CTX(CTX, T) \
  instance (CTX, MonadBaseControl b m) => MonadBaseControl b (T m) where BODY(T)

TRANS(IdentityT)
TRANS(MaybeT)
TRANS(ListT)
TRANS(ReaderT r)
TRANS(Strict.StateT s)
TRANS(       StateT s)
TRANS(ExceptT e)

TRANS_CTX(Error e,         ErrorT e)
TRANS_CTX(Monoid w, Strict.WriterT w)
TRANS_CTX(Monoid w,        WriterT w)
TRANS_CTX(Monoid w, Strict.RWST r w s)
TRANS_CTX(Monoid w,        RWST r w s)


--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- | An often used composition: @control f = 'liftBaseWith' f >>= 'restoreM'@
control :: MonadBaseControl b m => (RunInBase m b -> b (StM m a)) -> m a
control f = liftBaseWith f >>= restoreM
{-# INLINABLE control #-}

-- | Embed a transformer function as an function in the base monad returning a
-- mutated transformer state.
embed :: MonadBaseControl b m => (a -> m c) -> m (a -> b (StM m c))
embed f = liftBaseWith $ \runInBase -> return (runInBase . f)
{-# INLINABLE embed #-}

-- | Performs the same function as 'embed', but discards transformer state
-- from the embedded function.
embed_ :: MonadBaseControl b m => (a -> m ()) -> m (a -> b ())
embed_ f = liftBaseWith $ \runInBase -> return (void . runInBase . f)
{-# INLINABLE embed_ #-}

-- | Capture the current state of a transformer
captureT :: (MonadTransControl t, Monad (t m), Monad m) => t m (StT t ())
captureT = liftWith $ \runInM -> runInM (return ())
{-# INLINABLE captureT #-}

-- | Capture the current state above the base monad
captureM :: MonadBaseControl b m => m (StM m ())
captureM = liftBaseWith $ \runInBase -> runInBase (return ())
{-# INLINABLE captureM #-}

-- | @liftBaseOp@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @((a -> b c) -> b c)@ to: @('MonadBaseControl' b m => (a -> m c) -> m c)@.
--
-- For example:
--
-- @liftBaseOp alloca :: 'MonadBaseControl' 'IO' m => (Ptr a -> m c) -> m c@
liftBaseOp :: MonadBaseControl b m
           => ((a -> b (StM m c)) -> b (StM m d))
           -> ((a ->        m c)  ->        m d)
liftBaseOp f = \g -> control $ \runInBase -> f $ runInBase . g
{-# INLINABLE liftBaseOp #-}

-- | @liftBaseOp_@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @(b a -> b a)@ to: @('MonadBaseControl' b m => m a -> m a)@.
--
-- For example:
--
-- @liftBaseOp_ mask_ :: 'MonadBaseControl' 'IO' m => m a -> m a@
liftBaseOp_ :: MonadBaseControl b m
            => (b (StM m a) -> b (StM m c))
            -> (       m a  ->        m c)
liftBaseOp_ f = \m -> control $ \runInBase -> f $ runInBase m
{-# INLINABLE liftBaseOp_ #-}

-- | @liftBaseDiscard@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @(b () -> b a)@ to: @('MonadBaseControl' b m => m () -> m a)@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the base monad @b@.
--
-- For example:
--
-- @liftBaseDiscard forkIO :: 'MonadBaseControl' 'IO' m => m () -> m ThreadId@
liftBaseDiscard :: MonadBaseControl b m => (b () -> b a) -> (m () -> m a)
liftBaseDiscard f = \m -> liftBaseWith $ \runInBase -> f $ void $ runInBase m
{-# INLINABLE liftBaseDiscard #-}

-- | @liftBaseOpDiscard@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @((a -> b ()) -> b c)@ to: @('MonadBaseControl' b m => (a -> m ()) -> m c)@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the base monad @b@.
--
-- For example:
--
-- @liftBaseDiscard (runServer addr port) :: 'MonadBaseControl' 'IO' m => m () -> m ()@
liftBaseOpDiscard :: MonadBaseControl b m
                  => ((a -> b ()) -> b c)
                  ->  (a -> m ()) -> m c
liftBaseOpDiscard f g = liftBaseWith $ \runInBase -> f $ void . runInBase . g
{-# INLINABLE liftBaseOpDiscard #-}

-- | Transform an action in @t m@ using a transformer that operates on the underlying monad @m@
liftThrough
    :: (MonadTransControl t, Monad (t m), Monad m)
    => (m (StT t a) -> m (StT t b)) -- ^
    -> t m a -> t m b
liftThrough f t = do
  st <- liftWith $ \run -> do
    f $ run t
  restoreT $ return st
