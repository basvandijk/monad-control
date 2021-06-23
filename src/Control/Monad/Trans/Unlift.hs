{-# LANGUAGE CPP
           , NoImplicitPrelude
           , RankNTypes
           , TypeFamilies
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , GADTs
           , TypeOperators
           , DefaultSignatures
           , ScopedTypeVariables
           , MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

#if MIN_VERSION_transformers(0,4,0)
-- Hide warnings for the deprecated ErrorT transformer:
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

{- |
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD3
Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>

This module defines the type class 'MonadBaseUnlift', a subset of
'MonadBase' into which generic control operations such as @catch@ can be
lifted from @IO@ or any other base monad. Instances are based on monad
transformers in 'MonadTransUnlift', which includes all standard monad
transformers in the @transformers@ library except @ContT@.

See the <http://hackage.haskell.org/package/lifted-base lifted-base>
package which uses @monad-control@ to lift @IO@
operations from the @base@ library (like @catch@ or @bracket@) into any monad
that is an instance of @MonadBase@ or @MonadBaseUnlift@.

See the following tutorial by Michael Snoyman on how to use this package:

<https://www.yesodweb.com/book/monad-control>

=== Quick implementation guide

Given a base monad @B@ and a stack of transformers @T@:

* Define instances @'MonadTransUnlift' T@ for all transformers @T@, using the
  @'defaultLiftWith'@ and @'defaultRestoreT'@ functions on the constructor and
  deconstructor of @T@.

* Define an instance @'MonadBaseUnlift' B B@ for the base monad:

    @
    instance MonadBaseUnlift B B where
        type StM B a   = a
        withRunInBase f = f 'id'
        restoreM       = 'return'
    @

* Define instances @'MonadBaseUnlift' B m => 'MonadBaseUnlift' B (T m)@ for
  all transformers:

    @
    instance MonadBaseUnlift b m => MonadBaseUnlift b (T m) where
        type StM (T m) a = 'ComposeSt' T m a
        withRunInBase f   = 'defaultWithRunInBase'
        restoreM         = 'defaultRestoreM'
    @
-}

module Control.Monad.Trans.Unlift
    ( -- * MonadTransUnlift
      MonadTransUnlift(..), Unlift

      -- ** Defaults
      -- $MonadTransUnliftDefaults
    , defaultLiftWith, defaultRestoreT
      -- *** Defaults for a stack of two
      -- $MonadTransUnliftDefaults2
    , Unlift2, defaultLiftWith2, defaultRestoreT2

      -- * MonadBaseUnlift
    , MonadBaseUnlift (..), RunInBase

      -- ** Defaults
      -- $MonadBaseUnliftDefaults
    , RunInBaseDefault, defaultWithRunInBase

      -- * Utility functions
    , control, controlT, embed, embed_, captureT, captureM

    , liftBaseOp, liftBaseOp_

    , liftBaseDiscard, liftBaseOpDiscard

    , liftThrough
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( (.), ($), const )
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

import Control.Monad.Trans.Identity ( IdentityT(IdentityT), runIdentityT )
import Control.Monad.Trans.Reader   ( ReaderT  (ReaderT),   runReaderT )

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

import Control.Monad.Trans.Control (MonadTransControl (..), MonadBaseControl (..))
import Data.Type.Equality ((:~:) (..))
import Data.Proxy (Proxy (..))

--------------------------------------------------------------------------------
-- MonadTransUnlift type class
--------------------------------------------------------------------------------

-- | The @MonadTransUnlift@ type class is a stronger version of @'MonadTrans'@:
--
-- Instances of @'MonadTrans'@ know how to @'lift'@ actions in the base monad to
-- the transformed monad. These lifted actions, however, are completely unaware
-- of the monadic state added by the transformer.
--
-- @'MonadTransUnlift'@ instances are aware of the monadic state of the
-- transformer and allow to save and restore this state.
--
-- This allows to lift functions that have a monad transformer in both positive
-- and negative position. Take, for example, the function
--
-- @
-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
-- @
--
-- @'MonadTrans'@ instances can only lift the return type of the @withFile@
-- function:
--
-- @
-- withFileLifted :: MonadTrans t => FilePath -> IOMode -> (Handle -> IO r) -> t IO r
-- withFileLifted file mode action = lift (withFile file mode action)
-- @
--
-- However, @'MonadTrans'@ is not powerful enough to make @withFileLifted@
-- accept a function that returns @t IO@. The reason is that we need to take
-- away the transformer layer in order to pass the function to @'withFile'@.
-- @'MonadTransUnlift'@ allows us to do this:
--
-- @
-- withFileLifted' :: (Monad (t IO), MonadTransUnlift t) => FilePath -> IOMode -> (Handle -> t IO r) -> t IO r
-- withFileLifted' file mode action = withUnlift (\\run -> withFile file mode (run . action)) >>= restoreT . return
-- @
class MonadTransControl t => MonadTransUnlift t where
  -- | Monadic state of @t@.
  --
  -- The monadic state of a monad transformer is the result type of its @run@
  -- function, e.g.:
  --
  -- @
  -- 'runReaderT' :: 'ReaderT' r m a -> r -> m a
  -- 'StT' ('ReaderT' r) a ~ a
  --
  -- 'runStateT' :: 'StateT' s m a -> s -> m (a, s)
  -- 'StT' ('StateT' s) a ~ (a, s)
  --
  -- 'runMaybeT' :: 'MaybeT' m a -> m ('Maybe' a)
  -- 'StT' 'MaybeT' a ~ 'Maybe' a
  -- @
  --
  sttIsId :: Proxy t -> StT t a :~: a
  default sttIsId :: (StT t a ~ a) => Proxy t -> StT t a :~: a
  sttIsId _ = Refl

  -- | @withUnlift@ is similar to 'lift' in that it lifts a computation from
  -- the argument monad to the constructed monad.
  --
  -- Instances should satisfy similar laws as the 'MonadTrans' laws:
  --
  -- @withUnlift (\\_ -> return a) = return a@
  --
  -- @withUnlift (\\_ -> m >>= f)  =  withUnlift (\\_ -> m) >>= (\\a -> withUnlift (\\_ -> f a))@
  --
  -- The difference with 'lift' is that before lifting the @m@ computation
  -- @withUnlift@ captures the state of @t@. It then provides the @m@
  -- computation with a 'Run' function that allows running @t n@ computations in
  -- @n@ (for all @n@) on the captured state, e.g.
  --
  -- @
  -- withFileLifted :: (Monad (t IO), MonadTransUnlift t) => FilePath -> IOMode -> (Handle -> t IO r) -> t IO r
  -- withFileLifted file mode action = withUnlift (\\run -> withFile file mode (run . action)) >>= restoreT . return
  -- @
  --
  -- If the @Unlift@ function is ignored, @withUnlift@ coincides with @lift@:
  --
  -- @lift f = withUnlift (\\_ -> f)@
  --
  -- Implementations use the @'Run'@ function associated with a transformer:
  --
  -- @
  -- withUnlift :: 'Monad' m => (('Monad' n => 'ReaderT' r n b -> n b) -> m a) -> 'ReaderT' r m a
  -- withUnlift f = 'ReaderT' (\\r -> f (\\action -> 'runReaderT' action r))
  -- @
  withUnlift :: Monad m => (Unlift t -> m a) -> t m a
  withUnlift action = liftWith (\unlift -> action (\ma -> coe $ unlift ma))
    where
      coe :: forall b n. n (StT t b) -> n b
      coe = case sttIsId (Proxy :: Proxy t) :: StT t b :~: b of
          Refl -> id

-- | A function that runs a transformed monad @t n@ on the monadic state that
-- was captured by 'withUnlift'
--
-- A @Run t@ function yields a computation in @n@ that returns the monadic state
-- of @t@. This state can later be used to restore a @t@ computation using
-- 'restoreT'.
--
-- Example type equalities:
--
-- @
-- Unlift 'IdentityT'    ~ forall n b. 'Monad' n => 'IdentityT'  n b -> n b
-- Unlift ('ReaderT' r)  ~ forall n b. 'Monad' n => 'ReaderT' r  n b -> n b
-- @
--
type Unlift t = forall n b. Monad n => t n b -> n b


--------------------------------------------------------------------------------
-- Defaults for MonadTransUnlift
--------------------------------------------------------------------------------

-- $MonadTransUnliftDefaults
--
-- The following functions can be used to define a 'MonadTransUnlift' instance
-- for a monad transformer which simply is a newtype around another monad
-- transformer which already has a @MonadTransUnlift@ instance. For example:
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
-- {-\# LANGUAGE UndecidableInstances \#-}
-- {-\# LANGUAGE TypeFamilies \#-}
--
-- newtype CounterT m a = CounterT {unCounterT :: StateT Int m a}
--   deriving (Monad, MonadTrans)
--
-- instance MonadTransUnlift CounterT where
--     type StT CounterT a = StT (StateT Int) a
--     withUnlift = 'defaultLiftWith' CounterT unCounterT
-- @

-- | Default definition for the 'withUnlift' method.
defaultLiftWith :: (Monad m, MonadTransUnlift n)
                => (forall b.   n m b -> t m b)     -- ^ Monad constructor
                -> (forall o b. t o b -> n o b)     -- ^ Monad deconstructor
                -> (Unlift t -> m a)
                -> t m a
defaultLiftWith t unT = \f -> t $ withUnlift $ \run -> f $ run . unT
{-# INLINABLE defaultLiftWith #-}

-- | Default definition for the 'restoreT' method.
defaultRestoreT :: (Monad m, MonadTransUnlift n)
                => (n m a -> t m a)     -- ^ Monad constructor
                -> m (StT n a)
                -> t m a
defaultRestoreT t = t . restoreT
{-# INLINABLE defaultRestoreT #-}

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- $MonadTransUnliftDefaults2
--
-- The following functions can be used to define a 'MonadTransUnlift' instance
-- for a monad transformer stack of two.
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
--
-- newtype CalcT m a = CalcT { unCalcT :: StateT Int (ExceptT String m) a }
--   deriving (Monad, MonadTrans)
--
-- instance MonadTransUnlift CalcT where
--     type StT CalcT a = StT (ExceptT String) (StT (StateT Int) a)
--     withUnlift = 'defaultLiftWith2' CalcT unCalcT
--     restoreT = 'defaultRestoreT2' CalcT
-- @

-- | A function like 'Run' that runs a monad transformer @t@ which wraps the
-- monad transformers @n@ and @n'@. This is used in 'defaultLiftWith2'.
type Unlift2 t n' = forall m b. (Monad m, Monad (n' m)) => t m b -> m b

-- | Default definition for the 'withUnlift' method.
defaultLiftWith2 :: (Monad m, Monad (n' m), MonadTransUnlift n, MonadTransUnlift n')
                 => (forall b.   n (n' m) b -> t m b)     -- ^ Monad constructor
                 -> (forall o b. t o b -> n (n' o) b)     -- ^ Monad deconstructor
                 -> (Unlift2 t n'  -> m a)
                 -> t m a
defaultLiftWith2 t unT = \f -> t $ withUnlift $ \run -> withUnlift $ \run' -> f $ run' . run . unT
{-# INLINABLE defaultLiftWith2 #-}

-- | Default definition for the 'restoreT' method for double 'MonadTransUnlift'.
defaultRestoreT2 :: (Monad m, Monad (n' m), MonadTransUnlift n, MonadTransUnlift n')
                 => (n (n' m) a -> t m a)     -- ^ Monad constructor
                 -> m (StT n' (StT n a))
                 -> t m a
defaultRestoreT2 t = t . restoreT . restoreT
{-# INLINABLE defaultRestoreT2 #-}

--------------------------------------------------------------------------------
-- MonadTransUnlift instances
--------------------------------------------------------------------------------

instance MonadTransUnlift IdentityT where
    withUnlift f = IdentityT $ f $ runIdentityT
    {-# INLINABLE withUnlift #-}

instance MonadTransUnlift (ReaderT r) where
    withUnlift f = ReaderT $ \r -> f $ \t -> runReaderT t r
    {-# INLINABLE withUnlift #-}

--------------------------------------------------------------------------------
-- MonadBaseUnlift type class
--------------------------------------------------------------------------------

-- |
-- == Writing instances
--
-- The usual way to write a @'MonadBaseUnlift'@ instance for a transformer
-- stack over a base monad @B@ is to write an instance @MonadBaseUnlift B B@
-- for the base monad, and @MonadTransUnlift T@ instances for every transformer
-- @T@. Instances for @'MonadBaseUnlift'@ are then simply implemented using
-- @'ComposeSt'@, @'defaultWithRunInBase'@, @'defaultRestoreM'@.
class MonadBase b m => MonadBaseUnlift b m | m -> b where
    -- | Monadic state that @m@ adds to the base monad @b@.
    --
    -- For all base (non-transformed) monads, @StM m a ~ a@:
    --
    -- @
    -- StM 'IO'         a ~ a
    -- StM 'Maybe'      a ~ a
    -- StM ('Either' e) a ~ a
    -- StM []         a ~ a
    -- StM ((->) r)   a ~ a
    -- StM 'Identity'   a ~ a
    -- StM 'STM'        a ~ a
    -- StM ('ST' s)     a ~ a
    -- @
    --
    -- If @m@ is a transformed monad, @m ~ t b@, @'StM'@ is the monadic state of
    -- the transformer @t@ (given by its 'StT' from 'MonadTransUnlift'). For a
    -- transformer stack, @'StM'@ is defined recursively:
    --
    -- @
    -- StM ('IdentityT'  m) a ~ 'ComposeSt' 'IdentityT' m a ~ StM m a
    -- StM ('MaybeT'     m) a ~ 'ComposeSt' 'MaybeT'    m a ~ StM m ('Maybe' a)
    -- StM ('ErrorT' e   m) a ~ 'ComposeSt' 'ErrorT'    m a ~ 'Error' e => StM m ('Either' e a)
    -- StM ('ExceptT' e  m) a ~ 'ComposeSt' 'ExceptT'   m a ~ StM m ('Either' e a)
    -- StM ('ListT'      m) a ~ 'ComposeSt' 'ListT'     m a ~ StM m [a]
    -- StM ('ReaderT' r  m) a ~ 'ComposeSt' 'ReaderT'   m a ~ StM m a
    -- StM ('StateT' s   m) a ~ 'ComposeSt' 'StateT'    m a ~ StM m (a, s)
    -- StM ('WriterT' w  m) a ~ 'ComposeSt' 'WriterT'   m a ~ 'Monoid' w => StM m (a, w)
    -- StM ('RWST' r w s m) a ~ 'ComposeSt' 'RWST'      m a ~ 'Monoid' w => StM m (a, s, w)
    -- @
    stmIsId :: Proxy a -> Proxy m -> StM m a :~: a
    default stmIsId :: (StM m a ~ a) => Proxy a -> Proxy m -> StM m a :~: a
    stmIsId _ _ = Refl

    -- | @withRunInBase@ is similar to 'liftIO' and 'liftBase' in that it
    -- lifts a base computation to the constructed monad.
    --
    -- Instances should satisfy similar laws as the 'MonadIO' and 'MonadBase' laws:
    --
    -- @withRunInBase (\\_ -> return a) = return a@
    --
    -- @withRunInBase (\\_ -> m >>= f)  =  withRunInBase (\\_ -> m) >>= (\\a -> withRunInBase (\\_ -> f a))@
    --
    -- As <https://stackoverflow.com/a/58106822/1477667 Li-yao Xia explains>, parametricity
    -- guarantees that
    --
    -- @f <$> withRunInBase q = withRunInBase $ \runInBase -> f <$> q runInBase@
    --
    -- The difference with 'liftBase' is that before lifting the base computation
    -- @withRunInBase@ captures the state of @m@. It then provides the base
    -- computation with a 'RunInBase' function that allows running @m@
    -- computations in the base monad on the captured state:
    --
    -- @
    -- withFileLifted :: MonadBaseUnlift IO m => FilePath -> IOMode -> (Handle -> m a) -> m a
    -- withFileLifted file mode action = withRunInBase (\\runInBase -> withFile file mode (runInBase . action)) >>= restoreM
    --                              -- = control $ \\runInBase -> withFile file mode (runInBase . action)
    --                              -- = liftBaseOp (withFile file mode) action
    -- @
    --
    -- @'withRunInBase'@ is usually not implemented directly, but using
    -- @'defaultWithRunInBase'@.
    withRunInBase :: (RunInBase m b -> b a) -> m a

-- | A function that runs a @m@ computation on the monadic state that was
-- captured by 'withRunInBase'
--
-- A @RunInBase m@ function yields a computation in the base monad of @m@ that
-- returns the monadic state of @m@. This state can later be used to restore the
-- @m@ computation using 'restoreM'.
--
-- Example type equalities:
--
-- @
-- RunInBase ('IdentityT'  m) b ~ forall a.             'IdentityT'  m a -> b ('StM' m a)
-- RunInBase ('MaybeT'     m) b ~ forall a.             'MaybeT'     m a -> b ('StM' m ('Maybe' a))
-- RunInBase ('ErrorT' e   m) b ~ forall a. 'Error' e =>  'ErrorT' e   m a -> b ('StM' m ('Either' e a))
-- RunInBase ('ExceptT' e  m) b ~ forall a.             'ExceptT' e  m a -> b ('StM' m ('Either' e a))
-- RunInBase ('ListT'      m) b ~ forall a.             'ListT'      m a -> b ('StM' m [a])
-- RunInBase ('ReaderT' r  m) b ~ forall a.             'ReaderT'    m a -> b ('StM' m a)
-- RunInBase ('StateT' s   m) b ~ forall a.             'StateT' s   m a -> b ('StM' m (a, s))
-- RunInBase ('WriterT' w  m) b ~ forall a. 'Monoid' w => 'WriterT' w  m a -> b ('StM' m (a, w))
-- RunInBase ('RWST' r w s m) b ~ forall a. 'Monoid' w => 'RWST' r w s m a -> b ('StM' m (a, s, w))
-- @
--
-- For a transformed base monad @m ~ t b@, @'RunInBase m b' ~ 'Run' t@.
type RunInBase m b = forall a. m a -> b a


--------------------------------------------------------------------------------
-- MonadBaseUnlift instances for all monads in the base library
--------------------------------------------------------------------------------

#define BASE(M)                          \
instance MonadBaseUnlift (M) (M) where { \
    withRunInBase f = f id;              \
    {-# INLINABLE withRunInBase #-};     \
    }                                    \

BASE(IO)
BASE(Maybe)
BASE(Either e)
BASE([])
BASE((->) r)
BASE(Identity)

BASE(STM)

BASE(Strict.ST s)
BASE(       ST s)

#undef BASE


--------------------------------------------------------------------------------
-- Defaults for MonadBaseUnlift
--------------------------------------------------------------------------------

-- $MonadBaseUnliftDefaults
--
-- Note that by using the following default definitions it's easy to make a
-- monad transformer @T@ an instance of 'MonadBaseUnlift':
--
-- @
-- instance MonadBaseUnlift b m => MonadBaseUnlift b (T m) where
--     type StM (T m) a = 'ComposeSt' T m a
--     withRunInBase     = 'defaultWithRunInBase'
--     restoreM         = 'defaultRestoreM'
-- @
--
-- Defining an instance for a base monad @B@ is equally straightforward:
--
-- @
-- instance MonadBaseUnlift B B where
--     type StM B a   = a
--     withRunInBase f = f 'id'
--     restoreM       = 'return'
-- @

-- | A function like 'RunInBase' that runs a monad transformer @t@ in its base
-- monad @b@. It is used in 'defaultWithRunInBase'.
type RunInBaseDefault t (m :: * -> *) b = forall a. t m a -> b a

-- | Default definition for the 'withRunInBase' method.
--
-- Note that it composes a 'withUnlift' of @t@ with a 'withRunInBase' of @m@ to
-- give a 'withRunInBase' of @t m@:
--
-- @
-- defaultWithRunInBase = \\f -> 'withUnlift' $ \\run ->
--                               'withRunInBase' $ \\runInBase ->
--                                 f $ runInBase . run
-- @
defaultWithRunInBase :: (MonadTransUnlift t, MonadBaseUnlift b m)
                    => (RunInBaseDefault t m b -> b a) -> t m a
defaultWithRunInBase = \f -> withUnlift $ \run ->
                              withRunInBase $ \runInBase ->
                                f $ runInBase . run
{-# INLINABLE defaultWithRunInBase #-}

--------------------------------------------------------------------------------
-- MonadBaseUnlift transformer instances
--------------------------------------------------------------------------------

#define BODY {                                                        \
    stmIsId a _ = case stmIsId a (Proxy :: Proxy m) of { Refl -> Refl; }; \
    withRunInBase = defaultWithRunInBase;                             \
    {-# INLINABLE withRunInBase #-};                                  \
    }

#define TRANS(T) \
  instance (MonadBaseUnlift b m) => MonadBaseUnlift b (T m) where BODY

TRANS(IdentityT)
TRANS(ReaderT r)

#undef TRANS
#undef BASE

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- | An often used composition: @control f = 'withRunInBase' f >>= 'restoreM'@
--
-- Example:
--
-- @
-- liftedBracket :: MonadBaseUnlift IO m => m a -> (a -> m b) -> (a -> m c) -> m c
-- liftedBracket acquire release action = control $ \\runInBase ->
--     bracket (runInBase acquire)
--             (\\saved -> runInBase (restoreM saved >>= release))
--             (\\saved -> runInBase (restoreM saved >>= action))
-- @
control :: MonadBaseUnlift b m => (RunInBase m b -> b a) -> m a
control f = withRunInBase f >>= return
{-# INLINABLE control #-}

-- | Lift a computation and restore the monadic state immediately:
-- @controlT f = 'withUnlift' f >>= 'restoreT' . return@.
controlT :: (MonadTransUnlift t, Monad (t m), Monad m)
         => (Unlift t -> m a) -> t m a
controlT f = withUnlift f >>= return
{-# INLINABLE controlT #-}

-- | Embed a transformer function as an function in the base monad returning a
-- mutated transformer state.
embed :: MonadBaseUnlift b m => (a -> m c) -> m (a -> b c)
embed f = withRunInBase $ \runInBase -> return (runInBase . f)
{-# INLINABLE embed #-}

-- | Performs the same function as 'embed', but discards transformer state
-- from the embedded function.
embed_ :: MonadBaseUnlift b m => (a -> m ()) -> m (a -> b ())
embed_ f = withRunInBase $ \runInBase -> return (void . runInBase . f)
{-# INLINABLE embed_ #-}

-- | Capture the current state of a transformer
captureT :: (MonadTransUnlift t, Monad (t m), Monad m) => t m ()
captureT = withUnlift $ \runInM -> runInM (return ())
{-# INLINABLE captureT #-}

-- | Capture the current state above the base monad
captureM :: MonadBaseUnlift b m => m ()
captureM = withRunInBase $ \runInBase -> runInBase (return ())
{-# INLINABLE captureM #-}

-- | @liftBaseOp@ is a particular application of 'withRunInBase' that allows
-- lifting control operations of type:
--
-- @((a -> b c) -> b c)@
--
-- to:
--
-- @('MonadBaseUnlift' b m => (a -> m c) -> m c)@
--
-- For example:
--
-- @liftBaseOp alloca :: (Storable a, 'MonadBaseUnlift' 'IO' m) => (Ptr a -> m c) -> m c@
liftBaseOp :: MonadBaseUnlift b m
           => ((a -> b c) -> b d)
           -> ((a -> m c) -> m d)
liftBaseOp f = \g -> control $ \runInBase -> f $ runInBase . g
{-# INLINABLE liftBaseOp #-}

-- | @liftBaseOp_@ is a particular application of 'withRunInBase' that allows
-- lifting control operations of type:
--
-- @(b a -> b a)@
--
-- to:
--
-- @('MonadBaseUnlift' b m => m a -> m a)@
--
-- For example:
--
-- @liftBaseOp_ mask_ :: 'MonadBaseUnlift' 'IO' m => m a -> m a@
liftBaseOp_ :: MonadBaseUnlift b m
            => (b a -> b c)
            -> (m a -> m c)
liftBaseOp_ f = \m -> control $ \runInBase -> f $ runInBase m
{-# INLINABLE liftBaseOp_ #-}

-- | @liftBaseDiscard@ is a particular application of 'withRunInBase' that allows
-- lifting control operations of type:
--
-- @(b () -> b a)@
--
-- to:
--
-- @('MonadBaseUnlift' b m => m () -> m a)@
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the base monad @b@.
--
-- For example:
--
-- @liftBaseDiscard forkIO :: 'MonadBaseUnlift' 'IO' m => m () -> m ThreadId@
liftBaseDiscard :: MonadBaseUnlift b m => (b () -> b a) -> (m () -> m a)
liftBaseDiscard f = \m -> withRunInBase $ \runInBase -> f $ void $ runInBase m
{-# INLINABLE liftBaseDiscard #-}

-- | @liftBaseOpDiscard@ is a particular application of 'withRunInBase' that allows
-- lifting control operations of type:
--
-- @((a -> b ()) -> b c)@
--
-- to:
--
-- @('MonadBaseUnlift' b m => (a -> m ()) -> m c)@
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the base monad @b@.
--
-- For example:
--
-- @liftBaseDiscard (runServer addr port) :: 'MonadBaseUnlift' 'IO' m => m () -> m ()@
liftBaseOpDiscard :: MonadBaseUnlift b m
                  => ((a -> b ()) -> b c)
                  ->  (a -> m ()) -> m c
liftBaseOpDiscard f g = withRunInBase $ \runInBase -> f $ void . runInBase . g
{-# INLINABLE liftBaseOpDiscard #-}

-- | Transform an action in @t m@ using a transformer that operates on the underlying monad @m@
liftThrough
    :: (MonadTransUnlift t, Monad (t m), Monad m)
    => (m a -> m b) -- ^
    -> t m a -> t m b
liftThrough f t = do
  st <- withUnlift $ \run -> do
    f $ run t
  return st
