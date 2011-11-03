{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.Timeout.Control
-- Copyright   :  (c) The University of Glasgow 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Attach a timeout event to monadic computations
-- which are instances of 'MonadControlIO'.
--
-------------------------------------------------------------------------------

module System.Timeout.Control ( timeout ) where

-- from base:
import           Data.Int            ( Int )
import           Data.Maybe          ( Maybe(Nothing, Just), maybe )
import           Control.Monad       ( (>>=), return, liftM )
import qualified System.Timeout as T ( timeout )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from monad-control (this package):
import Control.Monad.Trans.Control ( MonadControlIO, restore, liftControlIO )

-- | Generalized version of 'T.timeout'.
timeout ∷ MonadControlIO m ⇒ Int → m α → m (Maybe α)
timeout t m = liftControlIO (\runInIO → T.timeout t (runInIO m)) >>=
                maybe (return Nothing) (liftM Just ∘ restore)
{-# INLINABLE timeout #-}
