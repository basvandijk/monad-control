{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, RankNTypes #-}

{- |
Module      :  Control.Monad.Trans.Control
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
Portability :  Requires RankNTypes

This module defines the class 'MonadTransControl' of monad transformers through
which control operations can be lifted.  Instances are included for all the
standard monad transformers from the @transformers@ library except @ContT@.

Additionally this module defines the class 'MonadControlIO' of 'IO'-based monads
into which control operations on 'IO' (such as exception catching or memory
allocation) can be lifted.

'liftIOOp' and 'liftIOOp_' enable convenient lifting of two common special cases
of control operation types.

'idLiftControl' and 'liftLiftControlBase' are provided to assist creation of
'MonadControlIO'-like classes based on core monads other than 'IO'.
-}

module Control.Monad.Trans.Control
    ( -- * MonadTransControl type class
      MonadTransControl(..), Run

      -- * MonadControlIO type class
    , MonadControlIO(..), RunInBase

    , controlIO

    , liftIOOp, liftIOOp_

      -- * Lifting utilities
    , idLiftControl
    , liftLiftControlBase
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( ($) )
import Data.Monoid   ( Monoid, mempty )
import Control.Monad ( Monad, join, return, liftM )
import System.IO     ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from transformers:
import Control.Monad.Trans.Class    ( MonadTrans, lift )
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

@join $ liftControl $ \\run -> run t = t@
-}
class MonadTrans t ⇒ MonadTransControl t where
  liftControl ∷ Monad m ⇒ (Run t → m α) → t m α

type Run t = ∀ n o β
           . (Monad n, Monad o, Monad (t o))
           ⇒ t n β → n (t o β)


--------------------------------------------------------------------------------
-- MonadTransControl instances
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
          let run t = liftM (\ ~(x, s') → StateT $ \_ → return (x, s'))
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
          run t = liftM (\ ~(x, w) → WriterT $ return (x, w))
                        (runWriterT t)

instance Monoid w ⇒ MonadTransControl (Strict.WriterT w) where
    liftControl f = Strict.WriterT $ liftM (\x → (x, mempty)) (f run)
        where
          run t = liftM (\(x, w) → Strict.WriterT $ return (x, w))
                        (Strict.runWriterT t)

instance Monoid w ⇒ MonadTransControl (RWST r w s) where
    liftControl f =
        RWST $ \r s →
          let run t = liftM (\ ~(x, s', w) → RWST $ \_ _ → return (x, s', w))
                            (runRWST t r s)
          in liftM (\x → (x, s, mempty)) (f run)

instance Monoid w ⇒ MonadTransControl (Strict.RWST r w s) where
    liftControl f =
        Strict.RWST $ \r s →
          let run t = liftM (\(x, s', w) → Strict.RWST $ \_ _ → return (x, s', w))
                            (Strict.runRWST t r s)
          in liftM (\x → (x, s, mempty)) (f run)


--------------------------------------------------------------------------------
-- MonadControlIO type class
--------------------------------------------------------------------------------

{-|
Instances should satisfy similar laws as the 'MonadIO' laws:

@liftControlIO . const . return = return@

@liftControlIO (const (m >>= f)) = liftControlIO (const m) >>= liftControlIO . const . f@

Additionally instances should satisfy:

@join $ liftControlIO $ \\runInIO -> runInIO m = m@
-}
class MonadIO m ⇒ MonadControlIO m where
  liftControlIO ∷ (RunInBase m IO → IO α) → m α

type RunInBase m base = ∀ β. m β → base (m β)

-- | An often used composition: @controlIO = 'join' . 'liftControlIO'@
{-# INLINABLE controlIO #-}
controlIO ∷ MonadControlIO m ⇒ (RunInBase m IO → IO (m α)) → m α
controlIO = join ∘ liftControlIO


--------------------------------------------------------------------------------
-- MonadControlIO instances
--------------------------------------------------------------------------------

instance MonadControlIO IO where
    liftControlIO = idLiftControl

instance MonadControlIO m ⇒ MonadControlIO (IdentityT m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (ListT m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (MaybeT m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Error e, MonadControlIO m) ⇒ MonadControlIO (ErrorT e m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (ReaderT r m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (StateT s m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance MonadControlIO m ⇒ MonadControlIO (Strict.StateT s m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (WriterT w m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (Strict.WriterT w m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (RWST r w s m) where
    liftControlIO = liftLiftControlBase liftControlIO

instance (Monoid w, MonadControlIO m) ⇒ MonadControlIO (Strict.RWST r w s m) where
    liftControlIO = liftLiftControlBase liftControlIO


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
         ⇒ ((α → IO (m β)) → IO (m γ))
         → ((α →     m β)  →     m γ)
liftIOOp f = \g → controlIO $ \runInIO → f $ runInIO ∘ g

{-|
@liftIOOp_@ is a particular application of 'liftControlIO' that allows
lifting control operations of type:

@('IO' a -> 'IO' a)@  (e.g. @mask_@) to:

@('MonadControlIO' m => m a -> m a)@.
-}
liftIOOp_ ∷ MonadControlIO m
          ⇒ (IO (m α) → IO (m β))
          → (    m α →      m β)
liftIOOp_ f = \m → controlIO $ \runInIO → f $ runInIO m


--------------------------------------------------------------------------------
-- Lifting utilties
--------------------------------------------------------------------------------

{-|
@idLiftControl@ acts as the \"identity\" 'liftControl' operation from a monad
@m@ to itself:

@idLiftControl f = f $ 'liftM' 'return'@

It serves as the base case for a class like 'MonadControlIO', which
allows control operations in some base monad (here 'IO') to be
lifted through arbitrary stacks of zero or more monad transformers
in one call.  For example,

@
instance 'MonadControlIO' 'IO' where
    'liftControlIO' = 'idLiftControl'
@
-}
idLiftControl ∷ Monad m ⇒ (RunInBase m m → m α) → m α
idLiftControl f = f $ liftM return

{-|
@liftLiftControlBase@ is used to compose two 'liftControl' operations:
the outer provided by a 'MonadTransControl' instance,
and the inner provided as the argument.

It satisfies @liftLiftControlBase 'idLiftControl' = 'liftControl'@.

It serves as the induction step of a 'MonadControlIO'-like class. For
example:

@
instance 'MonadControlIO' m => 'MonadControlIO' ('StateT' s m) where
    'liftControlIO' = 'liftLiftControlBase' 'liftControlIO'
@

using the 'MonadTransControl' instance of @'StateT' s@.

The following shows the recursive structure of 'liftControlIO' applied to a
stack of three monad transformers with 'IO' as the base monad: @t1 (t2 (t3 'IO')) a@:

@
'liftControlIO'
 =
 'liftLiftControlBase' $
   'liftLiftControlBase' $
     'liftLiftControlBase' $
       'idLiftControl'
  =
  \\f ->
    'liftControl' $ \(run1 :: 'Run' t1) ->     -- Capture state of t1
      'liftControl' $ \(run2 :: 'Run' t2) ->   -- Capture state of t2
        'liftControl' $ \(run3 :: 'Run' t3) -> -- Capture state of t3
          let run :: 'RunInBase' (t1 (t2 (t3 'IO'))) 'IO'
              run = -- Restore state
                    ('liftM' ('join' . 'lift')  :: 'IO' (           t2 (t3 'IO') (t1 (t2 (t3 'IO')) a))   -> 'IO' (                       t1 (t2 (t3 'IO')) a))
                  . ('liftM' ('join' . 'lift')  :: 'IO' (    t3 'IO' (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a)))  -> 'IO' (           t2 (t3 'IO') (t1 (t2 (t3 'IO')) a)))
                    -- Identity conversion
                  . ('liftM' ('join' . 'lift')  :: 'IO' ('IO' (t3 'IO' (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a)))) -> 'IO' (    t3 'IO' (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a))))
                  . ('liftM' 'return'         :: 'IO' (    t3 'IO' (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a)))  -> 'IO' ('IO' (t3 'IO' (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a)))))
                    -- Run     (computation to run:)                            (inner monad:) (restore computation:)
                  . (run3 ::         t3 'IO'  (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a)) ->        'IO'      (t3 'IO' (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a))))
                  . (run2 ::     t2 (t3 'IO')             (t1 (t2 (t3 'IO')) a)  ->     t3 'IO'             (t2 (t3 'IO') (t1 (t2 (t3 'IO')) a)))
                  . (run1 :: t1 (t2 (t3 'IO'))                             a   -> t2 (t3 'IO')                        (t1 (t2 (t3 'IO')) a))
          in f run
@
-}
liftLiftControlBase ∷ (MonadTransControl t, Monad (t m), Monad m, Monad base)
                    ⇒ ((RunInBase m     base → base α) →   m α) -- ^ @liftControlBase@ operation
                    → ((RunInBase (t m) base → base α) → t m α)
liftLiftControlBase lftCtrlBase =
    \f → liftControl $ \run1 →
           lftCtrlBase $ \runInBase →
              let run = liftM (join ∘ lift) ∘ runInBase ∘ run1
              in f run
