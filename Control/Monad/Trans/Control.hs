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

{-|
@MonadTransControl@ is the class of monad transformers supporting an
extra operation 'liftControl', enabling control operations (functions that
use monadic actions as input instead of just output) to be lifted
through the transformer.
-}
class MonadTrans t ⇒ MonadTransControl t where
  {-|
  @liftControl@ is used to peel off the outer layer of a transformed
  monadic action, allowing an transformed action @t m a@ to be
  treated as a base action @m a@.

  More precisely, @liftControl@ captures the monadic state of @t@ at the
  point where it is bound (in @t m@), yielding a function of type:

  @'Run' t = forall n o b. (Monad n, Monad o) => t n b -> n (t o b)@

  This function runs a transformed monadic action @t n b@
  in the inner monad @n@ using the captured state, and leaves the
  result @t o b@ in the monad @n@ after all side effects in @n@
  have occurred.

  This can be used to lift control operations with types such as
  @M a -> M a@ into the transformed monad @t M@:

  @
  instance Monad M
  foo :: M a -> M a
  foo' :: ('MonadTransControl' t, 'Monad' (t M)) => t M a -> t M a
  foo' a = 'control' $ \run ->    -- run :: t M a -> M (t M a)
             foo $ run a       -- uses foo :: M (t M a) -> M (t M a)
  @

  Instances should satisfy similar laws as the 'MonadTrans' laws:

  @liftControl . const . return = return@

  @liftControl (const (m >>= f)) = liftControl (const m) >>= liftControl . const . f@

  Additionally instances should satisfy:

  @'control' $ \\run -> run t = t@
  -}
  liftControl ∷ Monad m ⇒ (Run t → m α) → t m α

type Run t = ∀ n o β
           . (Monad n, Monad o, Monad (t o))
           ⇒ t n β → n (t o β)

-- | An often used composition: @control = 'join' . 'liftControl'@
control ∷ (Monad m, Monad (t m), MonadTransControl t)
        ⇒ (Run t → m (t m α)) → t m α
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

liftControlNoState ∷ (Monad m, Monad f)
                   ⇒ (∀ p β. p (f β) → t p β)
                   → (∀ n β. t n β → n (f β))
                   → ((Run t → m α) → t m α)
liftControlNoState mkT runT = \f → mkT $ liftM return $ f $
                                     liftM (mkT ∘ return) ∘ runT

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

{-|
@idLiftControl@ acts as the \"identity\" 'liftControl' operation from a monad
@m@ to itself.

@idLiftControl f = f $ liftM return@

It serves as the base case for a class like @MonadControlIO@, which
allows control operations in some base monad (here @IO@) to be
lifted through arbitrary stacks of zero or more monad transformers
in one call.  For example, "Control.Monad.IO.Control" defines:

@
class MonadIO m => MonadControlIO m where
    liftControlIO :: (RunInBase m IO -> IO b) -> m b
@

@
instance MonadControlIO IO where
    liftControlIO = idLiftControl
@
-}
idLiftControl ∷ Monad m ⇒ (RunInBase m m → m α) → m α
idLiftControl f = f $ liftM return

type RunInBase m base = ∀ β. m β → base (m β)

{-|
@liftLiftControlBase@ is used to compose two 'liftControl' operations:
the outer provided by a 'MonadTransControl' instance,
and the inner provided as the argument.

It satisfies @'liftLiftControlBase' 'idLiftControl' = 'liftControl'@.

It serves as the induction step of a @MonadControlIO@-like class.  For
example, "Control.Monad.IO.Control" defines:

@
instance MonadControlIO m => MonadControlIO (StateT s m) where
    liftControlIO = liftLiftControlBase liftControlIO
@

using the 'MonadTransControl' instance of @'StateT' s@.

The following shows the recursive structure of 'liftControlIO' applied to a
stack of three monad transformers with IO as the base monad: @t1 (t2 (t3 IO)) a@:

@
liftControlIO
 =
 'liftLiftControlBase' $
   'liftLiftControlBase' $
     'liftLiftControlBase' $
       'idLiftControl'
  =
   \\f -> 'liftControl' $ \\run1 ->     -- Capture state of t1, run1 :: 'Run' t1
           'liftControl' $ \\run2 ->   -- Capture state of t2, run2 :: 'Run' t2
             'liftControl' $ \\run3 -> -- Capture state of t3, run3 :: 'Run' t3
               let run :: 'RunInBase' (t1 (t2 (t3 IO))) IO
                   run = -- Restore state
                         'liftM' ('join' . 'lift') -- :: IO (           t2 (t3 IO) (t1 (t2 (t3 IO)) a))   -> IO (                       t1 (t2 (t3 IO)) a)
                       . 'liftM' ('join' . 'lift') -- :: IO (    t3 IO (t2 (t3 IO) (t1 (t2 (t3 IO)) a)))  -> IO (           t2 (t3 IO) (t1 (t2 (t3 IO)) a))
                         -- Identity conversion
                       . 'liftM' ('join' . 'lift') -- :: IO (IO (t3 IO (t2 (t3 IO) (t1 (t2 (t3 IO)) a)))) -> IO (    t3 IO (t2 (t3 IO) (t1 (t2 (t3 IO)) a)))
                       . 'liftM' 'return'        -- :: IO (    t3 IO (t2 (t3 IO) (t1 (t2 (t3 IO)) a)))  -> IO (IO (t3 IO (t2 (t3 IO) (t1 (t2 (t3 IO)) a))))
                         -- Run     (computation to run:)                              (inner monad:) (restore computation:)
                       . run3 -- ::         t3 IO  (t2 (t3 IO) (t1 (t2 (t3 IO)) a)) ->        IO      (t3 IO (t2 (t3 IO) (t1 (t2 (t3 IO)) a)))
                       . run2 -- ::     t2 (t3 IO)             (t1 (t2 (t3 IO)) a)  ->     t3 IO             (t2 (t3 IO) (t1 (t2 (t3 IO)) a))
                       . run1 -- :: t1 (t2 (t3 IO))                             a   -> t2 (t3 IO)                        (t1 (t2 (t3 IO)) a)
               in f run
@
-}
liftLiftControlBase ∷ (MonadTransControl t, Monad (t m), Monad m, Monad base)
                    ⇒ ((RunInBase m     base → base α) →   m α) -- ^ @liftControlBase@ operation
                    → ((RunInBase (t m) base → base α) → t m α)
liftLiftControlBase lftCtrlBase = \f → liftControl $ \run1 →
                                         lftCtrlBase $ \runInBase →
                                           let run = liftM (join ∘ lift) ∘ runInBase ∘ run1
                                           in f run


-- The End ---------------------------------------------------------------------
