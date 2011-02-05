{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, RankNTypes #-}

{- |
Module      :  Control.Monad.Trans.Control
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
Portability :  Requires RankNTypes

This module defines the class 'MonadTransControl' of monad transformers
through which control operations can be lifted.  Instances are
included for all the standard monad transformers from the
@transformers@ library except @ContT@.

'idLiftControl' and 'liftLiftControlBase' are provided to assist creation of
@MonadControlIO@-like classes (see "Control.Monad.IO.Control") based on core
monads other than 'IO'.
-}

module Control.Monad.Trans.Control
    ( -- * MonadTransControl
      MonadTransControl(..)
    , Run
    , control

      -- * Lifting
    , idLiftControl
    , RunInBase
    , liftLiftControlBase
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( ($) )
import Data.Monoid   ( Monoid, mempty )
import Control.Monad ( Monad, join, return, liftM )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.Trans.Class    ( MonadTrans, lift )

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
-- MonadTransControl
--------------------------------------------------------------------------------

-- |@MonadTransControl@ is the class of monad transformers supporting an
-- extra operation 'liftControl', enabling control operations (functions that
-- use monadic actions as input instead of just output) to be lifted
-- through the transformer.
class MonadTrans t ⇒ MonadTransControl t where
  -- |@liftControl@ is used to peel off the outer layer of a transformed
  -- monadic action, allowing an transformed action @t m a@ to be
  -- treated as a base action @m b@.
  --
  -- More precisely, @liftControl@ captures the monadic state of @t@ at the
  -- point where it is bound (in @t n@), yielding a function of type
  -- @'Run' t n o = t n a -> n (t o a)@
  -- this function runs a transformed monadic action @t n a@
  -- in the base monad @n@ using the captured state, and leaves the
  -- result @t o a@ in the monad @n@ after all side effects in @n@
  -- have occurred.
  --
  -- This can be used to lift control operations with types such as
  -- @M a -> M a@ into the transformed monad @t M@:
  --
  -- @
  -- instance Monad M
  -- foo :: M a -> M a
  -- foo' :: ('MonadTransControl' t, 'Monad' (t M)) => t M a -> t M a
  -- foo' a = 'control' $ \run ->    -- run :: t M a -> M (t M a)
  --            foo $ run a       -- uses foo :: M (t M a) -> M (t M a)
  -- @
  --
  -- @liftControl@ is typically used with @m == n == o@, but is required to
  -- be polymorphic for greater type safety: for example, this type
  -- ensures that the result of running the action in @m@ has no
  -- remaining side effects in @m@.
  liftControl ∷ (Monad m, Monad n, Monad o) ⇒ (Run t n o → m a) → t m a

type Run t n o = ∀ b. t n b → n (t o b)

-- | An often used composition: @control = 'join' . 'liftControl'@
control ∷ (Monad n, Monad m, Monad o, Monad (t m), MonadTransControl t)
        ⇒ (Run t n o → m (t m a)) → t m a
control = join ∘ liftControl


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadTransControl IdentityT where
    liftControl f = IdentityT $ f run
        where
          run t = liftM return (runIdentityT t)

instance Error e ⇒
         MonadTransControl (ErrorT e) where liftControl = liftControlNoState ErrorT runErrorT
instance MonadTransControl ListT      where liftControl = liftControlNoState ListT  runListT
instance MonadTransControl MaybeT     where liftControl = liftControlNoState MaybeT runMaybeT

liftControlNoState ∷ (Monad m, Monad n, Monad o, Monad f)
                   ⇒ (∀ a p. p (f a) → t p a)
                   → (∀ a. t m a → m (f a))
                   → (Run t m o → n b) → t n b
liftControlNoState mkT runT = \f → mkT $ liftM return $ f run
    where
      run = liftM (mkT ∘ return) ∘ runT

instance MonadTransControl (ReaderT r) where
    liftControl f =
        ReaderT $ \r →
          let run t = liftM return (runReaderT t r)
          in f run

instance MonadTransControl (StateT s) where
    liftControl f =
        StateT $ \s →
          let run t = liftM (\(x, s') → StateT $ \_ → return (x, s'))
                            (runStateT t s)
          in liftM (\x → (x, s)) (f run)

instance MonadTransControl (Strict.StateT s) where
    liftControl f =
        Strict.StateT $ \s →
          let run t = liftM (\(x, s') → Strict.StateT $ \_ → return (x, s'))
                            (Strict.runStateT t s)
          in liftM (\x → (x, s)) (f run)

instance Monoid w ⇒ MonadTransControl (WriterT w) where
    liftControl f = WriterT $ liftM (\x → (x, mempty)) (f run)
        where
          run t = liftM (WriterT ∘ return) (runWriterT t)

instance Monoid w ⇒ MonadTransControl (Strict.WriterT w) where
    liftControl f = Strict.WriterT $ liftM (\x → (x, mempty)) (f run)
        where
          run t = liftM (Strict.WriterT ∘ return) (Strict.runWriterT t)

instance Monoid w ⇒ MonadTransControl (RWST r w s) where
    liftControl f =
        RWST $ \r s →
          let run t = liftM (\(x, s', w) → RWST $ \_ _ → return (x, s', w))
                            (runRWST t r s)
          in liftM (\x → (x, s, mempty)) (f run)

instance Monoid w ⇒ MonadTransControl (Strict.RWST r w s) where
    liftControl f =
        Strict.RWST $ \r s →
          let run t = liftM (\(x, s', w) → Strict.RWST $ \_ _ → return (x, s', w))
                            (Strict.runRWST t r s)
          in liftM (\x → (x, s, mempty)) (f run)


--------------------------------------------------------------------------------
-- Lifting
--------------------------------------------------------------------------------

-- |@idLiftControl@ acts as the \"identity\" 'liftControl' operation from a monad
-- @m@ to itself.
--
-- @idLiftControl f = f $ liftM return@
--
-- It serves as the base case for a class like @MonadControlIO@, which
-- allows control operations in some base monad (here @IO@) to be
-- lifted through arbitrary stacks of zero or more monad transformers
-- in one call.  For example, "Control.Monad.IO.Control" defines
--
-- @
-- class MonadIO m => MonadControlIO m where
--     liftControlIO :: (RunInBase m IO -> IO b) -> m b
-- @
--
-- @
-- instance MonadControlIO IO where
--     liftControlIO = idLiftControl
-- @
idLiftControl ∷ Monad m ⇒ ((∀ b. m b → m (m b)) → m a) → m a
idLiftControl f = f $ liftM return

type RunInBase m base = ∀ b. m b → base (m b)

-- |@liftLiftControlBase@ is used to compose two 'liftControl' operations:
-- the outer provided by a 'MonadTransControl' instance,
-- and the inner provided as the argument.
--
-- It satisfies @'liftLiftControlBase' 'idLiftControl' == 'liftControl'@.
--
-- It serves as the induction step of a @MonadControlIO@-like class.  For
-- example, "Control.Monad.IO.Control" defines
--
-- @
-- instance MonadControlIO m => MonadControlIO (StateT s m) where
--     liftControlIO = liftLiftControlBase liftControlIO
-- @
--
-- using the 'MonadTransControl' instance of @'StateT' s@.
liftLiftControlBase ∷ (MonadTransControl t, Monad base, Monad m, Monad (t m))
                    ⇒ ((RunInBase m     base → base a) →   m a) -- ^ @liftControlBase@ operation
                    → ((RunInBase (t m) base → base a) → t m a)
liftLiftControlBase lftCtrlBase = \f → liftControl $ \run →
                                         lftCtrlBase $ \runInBase →
                                           f $ liftM (join ∘ lift) ∘ runInBase ∘ run


-- The End ---------------------------------------------------------------------
