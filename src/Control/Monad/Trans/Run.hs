{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Run
-- Copyright   :  (c) Jan Bessai 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  jan.bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable (depends on ghc)
--
-- Class to unify the run method of monad transfomers.
--
-- Each monad transformer also comes with an operation @run@/XXX/ to
-- unwrap the transformer, exposing a computation of the inner monad.
-- @Run@ exposes this operation as an interface.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Run ( Run (..) ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Lazy as RWSLazy
import qualified Control.Monad.Trans.RWS.Strict as RWSStrict
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict

import Data.Monoid

class (MonadTrans t) => Run t where
  -- | The type of the entity wrapped in the transformer
  type UnWrapped t m a

  -- | Runs the transfomer.
  -- This should usually be an alias to 'runXXX' of the transformer
  -- datatype.
  run :: (Monad m) => t m a -> UnWrapped t m a

instance Run (ContT r) where
  type UnWrapped (ContT r) m a = (a -> m r) -> m r
  run = runContT

instance (Error e) => Run (ErrorT e) where
  type UnWrapped (ErrorT e) m a = m (Either e a)
  run = runErrorT

instance Run IdentityT where
  type UnWrapped IdentityT m a = m a
  run = runIdentityT

instance Run ListT where
  type UnWrapped ListT m a = m [a]
  run = runListT

instance Run MaybeT where
  type UnWrapped MaybeT m a = m (Maybe a)
  run = runMaybeT

instance (Monoid w) => Run (RWSLazy.RWST r w s) where
  type UnWrapped (RWSLazy.RWST r w s) m a = r -> s -> m (a, s, w)
  run = RWSLazy.runRWST

instance (Monoid w) => Run (RWSStrict.RWST r w s) where
  type UnWrapped (RWSStrict.RWST r w s) m a = r -> s -> m (a, s, w)
  run = RWSStrict.runRWST

instance Run (ReaderT r) where
  type UnWrapped (ReaderT r) m a = r -> m a
  run = runReaderT

instance Run (StateLazy.StateT s) where
  type UnWrapped (StateLazy.StateT s) m a = s -> m (a, s)
  run = StateLazy.runStateT

instance Run (StateStrict.StateT s) where
  type UnWrapped (StateStrict.StateT s) m a = s -> m (a, s)
  run = StateStrict.runStateT

instance (Monoid w) => Run (WriterLazy.WriterT w) where
  type UnWrapped (WriterLazy.WriterT w) m a = m (a, w)
  run = WriterLazy.runWriterT

instance (Monoid w) => Run (WriterStrict.WriterT w) where
  type UnWrapped (WriterStrict.WriterT w) m a = m (a, w)
  run = WriterStrict.runWriterT

