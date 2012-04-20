{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , RankNTypes
           , TypeFamilies
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , MultiParamTypeClasses
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
    , MonadBaseControl (..), RunInBase

      -- * Defaults for MonadBaseControl
      -- $defaults
    , ComposeSt, defaultLiftBaseWith, defaultRestoreM

      -- * Utility functions
    , control

    , liftBaseOp, liftBaseOp_

    , liftBaseDiscard
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( ($), const )
import Data.Monoid   ( Monoid, mempty )
import Control.Monad ( Monad, (>>=), return, liftM )
import System.IO     ( IO )
import Data.Maybe    ( Maybe )
import Data.Either   ( Either )

#if MIN_VERSION_base(4,3,0)
import GHC.Conc.Sync ( STM )
#endif

#if MIN_VERSION_base(4,4,0) || defined(INSTANCE_ST)
import           Control.Monad.ST.Lazy             ( ST )
import qualified Control.Monad.ST.Strict as Strict ( ST )
#endif

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

#if MIN_VERSION_base(4,3,0)
import Control.Monad ( void )
#else
import Data.Functor (Functor, fmap)
void ∷ Functor f ⇒ f a → f ()
void = fmap (const ())
#endif

--------------------------------------------------------------------------------
-- MonadTransControl type class
--------------------------------------------------------------------------------

class MonadTrans t ⇒ MonadTransControl t where
  -- | Monadic state of @t@.
  data StT t ∷ * → *

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
  liftWith ∷ Monad m ⇒ (Run t → m a) → t m a

  -- | Construct a @t@ computation from the monadic state of @t@ that is
  -- returned from a 'Run' function.
  --
  -- Instances should satisfy:
  --
  -- @liftWith (\\run -> run t) >>= restoreT . return = t@
  restoreT ∷ Monad m ⇒ m (StT t a) → t m a

-- | A function that runs a transformed monad @t n@ on the monadic state that
-- was captured by 'liftWith'
--
-- A @Run t@ function yields a computation in @n@ that returns the monadic state
-- of @t@. This state can later be used to restore a @t@ computation using
-- 'restoreT'.
type Run t = ∀ n b. Monad n ⇒ t n b → n (StT t b)


--------------------------------------------------------------------------------
-- MonadTransControl instances
--------------------------------------------------------------------------------

instance MonadTransControl IdentityT where
    newtype StT IdentityT a = StId {unStId ∷ a}
    liftWith f = IdentityT $ f $ liftM StId ∘ runIdentityT
    restoreT = IdentityT ∘ liftM unStId
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadTransControl MaybeT where
    newtype StT MaybeT a = StMaybe {unStMaybe ∷ Maybe a}
    liftWith f = MaybeT $ liftM return $ f $ liftM StMaybe ∘ runMaybeT
    restoreT = MaybeT ∘ liftM unStMaybe
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance Error e ⇒ MonadTransControl (ErrorT e) where
    newtype StT (ErrorT e) a = StError {unStError ∷ Either e a}
    liftWith f = ErrorT $ liftM return $ f $ liftM StError ∘ runErrorT
    restoreT = ErrorT ∘ liftM unStError
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadTransControl ListT where
    newtype StT ListT a = StList {unStList ∷ [a]}
    liftWith f = ListT $ liftM return $ f $ liftM StList ∘ runListT
    restoreT = ListT ∘ liftM unStList
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadTransControl (ReaderT r) where
    newtype StT (ReaderT r) a = StReader {unStReader ∷ a}
    liftWith f = ReaderT $ \r → f $ \t → liftM StReader $ runReaderT t r
    restoreT = ReaderT ∘ const ∘ liftM unStReader
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadTransControl (StateT s) where
    newtype StT (StateT s) a = StState {unStState ∷ (a, s)}
    liftWith f = StateT $ \s →
                   liftM (\x → (x, s))
                         (f $ \t → liftM StState $ runStateT t s)
    restoreT = StateT ∘ const ∘ liftM unStState
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadTransControl (Strict.StateT s) where
    newtype StT (Strict.StateT s) a = StState' {unStState' ∷  (a, s)}
    liftWith f = Strict.StateT $ \s →
                   liftM (\x → (x, s))
                         (f $ \t → liftM StState' $ Strict.runStateT t s)
    restoreT = Strict.StateT ∘ const ∘ liftM unStState'
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (WriterT w) where
    newtype StT (WriterT w) a = StWriter {unStWriter ∷ (a, w)}
    liftWith f = WriterT $ liftM (\x → (x, mempty))
                                 (f $ liftM StWriter ∘ runWriterT)
    restoreT = WriterT ∘ liftM unStWriter
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (Strict.WriterT w) where
    newtype StT (Strict.WriterT w) a = StWriter' {unStWriter' ∷ (a, w)}
    liftWith f = Strict.WriterT $ liftM (\x → (x, mempty))
                                        (f $ liftM StWriter' ∘ Strict.runWriterT)
    restoreT = Strict.WriterT ∘ liftM unStWriter'
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (RWST r w s) where
    newtype StT (RWST r w s) a = StRWS {unStRWS ∷ (a, s, w)}
    liftWith f = RWST $ \r s → liftM (\x → (x, s, mempty))
                                     (f $ \t → liftM StRWS $ runRWST t r s)
    restoreT mSt = RWST $ \_ _ → liftM unStRWS mSt
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance Monoid w ⇒ MonadTransControl (Strict.RWST r w s) where
    newtype StT (Strict.RWST r w s) a = StRWS' {unStRWS' ∷  (a, s, w)}
    liftWith f =
        Strict.RWST $ \r s → liftM (\x → (x, s, mempty))
                                   (f $ \t → liftM StRWS' $ Strict.runRWST t r s)
    restoreT mSt = Strict.RWST $ \_ _ → liftM unStRWS' mSt
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}


--------------------------------------------------------------------------------
-- MonadBaseControl type class
--------------------------------------------------------------------------------

class MonadBase b m ⇒ MonadBaseControl b m | m → b where
    -- | Monadic state of @m@.
    data StM m ∷ * → *

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
    liftBaseWith ∷ (RunInBase m b → b a) → m a

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a 'RunInBase' function.
    --
    -- Instances should satisfy:
    --
    -- @liftBaseWith (\\runInBase -> runInBase m) >>= restoreM = m@
    restoreM ∷ StM m a → m a

-- | A function that runs a @m@ computation on the monadic state that was
-- captured by 'liftBaseWith'
--
-- A @RunInBase m@ function yields a computation in the base monad of @m@ that
-- returns the monadic state of @m@. This state can later be used to restore the
-- @m@ computation using 'restoreM'.
type RunInBase m b = ∀ a. m a → b (StM m a)


--------------------------------------------------------------------------------
-- MonadBaseControl instances for all monads in the base library
--------------------------------------------------------------------------------

#define BASE(M, ST)                       \
instance MonadBaseControl (M) (M) where { \
    newtype StM (M) a = ST a;             \
    liftBaseWith f = f $ liftM ST;        \
    restoreM (ST x) = return x;           \
    {-# INLINE liftBaseWith #-};          \
    {-# INLINE restoreM #-}}

BASE(IO,          StIO)
BASE(Maybe,       St)
BASE(Either e,    StE)
BASE([],          StL)
BASE((→) r,       StF)
BASE(Identity,    StI)

#if MIN_VERSION_base(4,3,0)
BASE(STM,         StSTM)
#endif

#if MIN_VERSION_base(4,4,0) || defined(INSTANCE_ST)
BASE(Strict.ST s, StSTS)
BASE(       ST s, StST)
#endif

#undef BASE


--------------------------------------------------------------------------------
-- Defaults for MonadBaseControl
--------------------------------------------------------------------------------

-- $defaults
--
-- Note that by using the following default definitions it's easy to make a
-- monad transformer @T@ an instance of 'MonadBaseControl':
--
-- @
-- instance MonadBaseControl b m => MonadBaseControl b (T m) where
--     newtype StM (T m) a = StMT {unStMT :: 'ComposeSt' T m a}
--     liftBaseWith = 'defaultLiftBaseWith' StMT
--     restoreM     = 'defaultRestoreM'   unStMT
-- @
--
-- Defining an instance for a base monad @B@ is equally straightforward:
--
-- @
-- instance MonadBaseControl B B where
--     newtype StM B a = StMB {unStMB :: a}
--     liftBaseWith f  = f $ liftM  StMB
--     restoreM        = return . unStMB
-- @

-- | Handy type synonym that composes the monadic states of @t@ and @m@.
--
-- It can be used to define the 'StM' for new 'MonadBaseControl' instances.
type ComposeSt t m a = StM m (StT t a)

-- | Default defintion for the 'liftBaseWith' method.
--
-- Note that it composes a 'liftWith' of @t@ with a 'liftBaseWith' of @m@ to
-- give a 'liftBaseWith' of @t m@:
--
-- @
-- defaultLiftBaseWith stM = \\f -> 'liftWith' $ \\run ->
--                                   'liftBaseWith' $ \\runInBase ->
--                                     f $ liftM stM . runInBase . run
-- @
defaultLiftBaseWith ∷ (MonadTransControl t, MonadBaseControl b m)
                    ⇒ (∀ c. ComposeSt t m c → StM (t m) c) -- ^ 'StM' constructor
                    → ((RunInBase (t m) b  → b a) → t m a)
defaultLiftBaseWith stM = \f → liftWith $ \run →
                                 liftBaseWith $ \runInBase →
                                   f $ liftM stM ∘ runInBase ∘ run
{-# INLINE defaultLiftBaseWith #-}

-- | Default definition for the 'restoreM' method.
--
-- Note that: @defaultRestoreM unStM = 'restoreT' . 'restoreM' . unStM@
defaultRestoreM ∷ (MonadTransControl t, MonadBaseControl b m)
                ⇒ (StM (t m) a → ComposeSt t m a)  -- ^ 'StM' deconstructor
                → (StM (t m) a → t m a)
defaultRestoreM unStM = restoreT ∘ restoreM ∘ unStM
{-# INLINE defaultRestoreM #-}


--------------------------------------------------------------------------------
-- MonadBaseControl transformer instances
--------------------------------------------------------------------------------

#define BODY(T, ST, unST) {                              \
    newtype StM (T m) a = ST {unST ∷ ComposeSt (T) m a}; \
    liftBaseWith = defaultLiftBaseWith ST;               \
    restoreM     = defaultRestoreM   unST;               \
    {-# INLINE liftBaseWith #-};                         \
    {-# INLINE restoreM #-}}

#define TRANS(         T, ST, unST) \
  instance (     MonadBaseControl b m) ⇒ MonadBaseControl b (T m) where BODY(T, ST, unST)
#define TRANS_CTX(CTX, T, ST, unST) \
  instance (CTX, MonadBaseControl b m) ⇒ MonadBaseControl b (T m) where BODY(T, ST, unST)

TRANS(IdentityT,       StMId,     unStMId)
TRANS(MaybeT,          StMMaybe,  unStMMaybe)
TRANS(ListT,           StMList,   unStMList)
TRANS(ReaderT r,       StMReader, unStMReader)
TRANS(Strict.StateT s, StMStateS, unStMStateS)
TRANS(       StateT s, StMState,  unStMState)

TRANS_CTX(Error e,         ErrorT e,   StMError,   unStMError)
TRANS_CTX(Monoid w, Strict.WriterT w,  StMWriterS, unStMWriterS)
TRANS_CTX(Monoid w,        WriterT w,  StMWriter,  unStMWriter)
TRANS_CTX(Monoid w, Strict.RWST r w s, StMRWSS,    unStMRWSS)
TRANS_CTX(Monoid w,        RWST r w s, StMRWS,     unStMRWS)


--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- | An often used composition: @control f = 'liftBaseWith' f >>= 'restoreM'@
control ∷ MonadBaseControl b m ⇒ (RunInBase m b → b (StM m a)) → m a
control f = liftBaseWith f >>= restoreM
{-# INLINE control #-}

-- | @liftBaseOp@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @((a -> b c) -> b c)@ to: @('MonadBaseControl' b m => (a -> m c) -> m c)@.
--
-- For example:
--
-- @liftBaseOp alloca :: 'MonadBaseControl' 'IO' m => (Ptr a -> m c) -> m c@
liftBaseOp ∷ MonadBaseControl b m
           ⇒ ((a → b (StM m c)) → b (StM m d))
           → ((a →        m c)  →        m d)
liftBaseOp f = \g → control $ \runInBase → f $ runInBase ∘ g
{-# INLINE liftBaseOp #-}

-- | @liftBaseOp_@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @(b a -> b a)@ to: @('MonadBaseControl' b m => m a -> m a)@.
--
-- For example:
--
-- @liftBaseOp_ mask_ :: 'MonadBaseControl' 'IO' m => m a -> m a@
liftBaseOp_ ∷ MonadBaseControl b m
            ⇒ (b (StM m a) → b (StM m c))
            → (       m a  →        m c)
liftBaseOp_ f = \m → control $ \runInBase → f $ runInBase m
{-# INLINE liftBaseOp_ #-}

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
liftBaseDiscard ∷ MonadBaseControl b m ⇒ (b () → b a) → (m () → m a)
liftBaseDiscard f = \m → liftBaseWith $ \runInBase → f $ void $ runInBase m
{-# INLINE liftBaseDiscard #-}
