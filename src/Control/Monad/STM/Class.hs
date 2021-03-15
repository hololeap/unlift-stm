{-|
Module      : Control.Monad.STM.Class
Licesnse    : BSD-2
Stability   : experimental

A typeclass for monads which can execute 'STM' actions. (This is essentially
a copy of "Control.Monad.IO.Class", modified for the 'STM' monad.)
-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Control.Monad.STM.Class
    ( MonadSTM(..) ) where

import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.CPS as RWS.CPS
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Select
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.CPS as Writer.CPS
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict

-- | Analogous to 'MonadIO'. It allows one to use @'liftSTM'@ when 'STM' is at
--   the "bottom" of the monad transformer stack.
class Monad m => MonadSTM m where
    liftSTM :: STM a -> m a

instance MonadSTM STM where
    liftSTM = id

instance (Monoid w, MonadSTM m) => MonadSTM (AccumT w m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ContT r m) where
    liftSTM = lift . liftSTM

instance (Error e, MonadSTM m) => MonadSTM (ErrorT e m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ExceptT e m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (IdentityT m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ListT m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (MaybeT m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (RWS.CPS.RWST r w s m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (RWS.Lazy.RWST r w s m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (RWS.Strict.RWST r w s m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ReaderT r m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (SelectT r m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (State.Lazy.StateT s m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (State.Strict.StateT s m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Writer.CPS.WriterT w m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Writer.Lazy.WriterT w m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Writer.Strict.WriterT w m) where
    liftSTM = lift . liftSTM


