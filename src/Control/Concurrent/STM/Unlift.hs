{-|
Module      : Control.Concurrent.STM.Unlift
Licesnse    : BSD-2
Stability   : experimental

Lifted versions of functions from "Control.Concurrent.STM":

* This is meant to be a drop-in replacement for "Control.Concurrent.STM" and
  "UnliftIO.STM", for more streamlined use of 'STM' functions when using
  mtl-style monad transformer stacks.

* Functions that normally return a 'STM' result are abstracted to 'MonadSTM'.
  (Some functions necessarily require the more restrictive 'MonadUnliftSTM'
  typeclass due to 'STM' results appearing in negative positions of the 
  function signature.)

* This module also includes STM-related functions which return an 'IO' result.
  These are abstracted to 'MonadIO' or 'MonadUnliftIO' and are re-exported
  directly from "UnliftIO.STM".

-}

module Control.Concurrent.STM.Unlift
    ( -- * Core
      STM
    , UIO.atomically
    , retry
    , check
    , orElse
    , throwSTM
    , catchSTM
      -- * TVar
    , TVar
    , UIO.newTVarIO
    , UIO.readTVarIO
    , newTVar
    , readTVar
    , writeTVar
    , modifyTVar
    , modifyTVar'
    , modifyTVarM
    , modifyTVarM'
    , swapTVar
    , UIO.registerDelay
    , UIO.mkWeakTVar
      -- * TMVar
    , TMVar
    , newTMVar
    , newEmptyTMVar
    , UIO.newTMVarIO
    , UIO.newEmptyTMVarIO
    , takeTMVar
    , putTMVar
    , readTMVar
    , tryReadTMVar
    , swapTMVar
    , tryTakeTMVar
    , tryPutTMVar
    , isEmptyTMVar
    , UIO.mkWeakTMVar
      -- * TChan
    , TChan
    , newTChan
    , UIO.newTChanIO
    , newBroadcastTChan
    , UIO.newBroadcastTChanIO
    , dupTChan
    , cloneTChan
    , readTChan
    , tryReadTChan
    , peekTChan
    , tryPeekTChan
    , writeTChan
    , unGetTChan
    , isEmptyTChan
      -- * TQueue
    , newTQueue
    , UIO.newTQueueIO
    , readTQueue
    , tryReadTQueue
    , peekTQueue
    , tryPeekTQueue
    , writeTQueue
    , unGetTQueue
    , isEmptyTQueue
      -- * TBQueue
    , newTBQueue
    , UIO.newTBQueueIO
    , readTBQueue
    , tryReadTBQueue
    , peekTBQueue
    , tryPeekTBQueue
    , writeTBQueue
    , unGetTBQueue
    , isEmptyTBQueue
    , isFullTBQueue
      -- * Low-level functions on file descriptors
    , threadWaitReadSTM
    , threadWaitWriteSTM
      -- * Re-exported modules
    , module Control.Monad.STM.Class
    , module Control.Monad.STM.Unlift
    , module Control.Monad.IO.Class
    , module Control.Monad.IO.Unlift
    ) where

import Control.Exception.Base (Exception)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.STM.Class
import Control.Monad.STM.Unlift
import Data.Bifunctor
import qualified GHC.Conc
import GHC.Natural (Natural)
import System.Posix.Types (Fd)

import           Control.Concurrent.STM (STM, TVar, TMVar, TChan, TQueue, TBQueue)
import qualified Control.Concurrent.STM as STM
import qualified UnliftIO.STM as UIO

-- Core

-- | Lifted version of 'STM.retry'
retry :: MonadSTM m => m a
retry = liftSTM STM.retry

-- | Lifted version of 'STM.check'
check :: MonadSTM m => Bool -> m ()
check = liftSTM . STM.check

-- | Lifted version of 'STM.orElse'
orElse :: MonadUnliftSTM m => m a -> m a -> m a
orElse i t = askRunInSTM >>= \conv -> liftSTM (STM.orElse (conv i) (conv t))

-- | Lifted version of 'STM.throwSTM'
throwSTM :: (Exception e, MonadSTM m) => e -> m a
throwSTM = liftSTM . STM.throwSTM

-- | Lifted version of 'STM.catchSTM'
catchSTM :: (Exception e, MonadUnliftSTM m) => m a -> (e -> m a) -> m a
catchSTM m f = askRunInSTM >>= \conv -> liftSTM (STM.catchSTM (conv m) (conv . f))

-- | Lifted version of 'GHC.Conc.threadWaitReadSTM'
threadWaitReadSTM :: (MonadIO io, MonadSTM stm) => Fd -> io (stm (), io ())
threadWaitReadSTM =
    liftIO . fmap (bimap liftSTM liftIO) . GHC.Conc.threadWaitReadSTM

-- | Lifted version of 'GHC.Conc.threadWaitWriteSTM'
threadWaitWriteSTM :: (MonadIO io, MonadSTM stm) => Fd -> io (stm (), io ())
threadWaitWriteSTM =
    liftIO . fmap (bimap liftSTM liftIO) . GHC.Conc.threadWaitWriteSTM

-- TVar

-- | Lifted version of 'STM.newTVar'
newTVar :: MonadSTM m => a -> m (TVar a)
newTVar = liftSTM . STM.newTVar

-- | Lifted version of 'STM.readTVar'
readTVar :: MonadSTM m => TVar a -> m a
readTVar = liftSTM . STM.readTVar

-- | Lifted version of 'STM.writeTVar'
writeTVar :: MonadSTM m => TVar a -> a -> m ()
writeTVar tvar = liftSTM . STM.writeTVar tvar

-- | Lifted version of 'STM.modifyTVar'
modifyTVar :: MonadSTM m => TVar a -> (a -> a) -> m ()
modifyTVar tvar = liftSTM . STM.modifyTVar tvar

-- | Lifted version of 'STM.modifyTVar''
modifyTVar' :: MonadSTM m => TVar a -> (a -> a) -> m ()
modifyTVar' tvar = liftSTM . STM.modifyTVar' tvar

-- | A monadic version of 'modifyTVar'
modifyTVarM :: MonadSTM m => TVar a -> (a -> m a) -> m ()
modifyTVarM tvar f = readTVar tvar >>= f >>= writeTVar tvar

-- | A monadic version of 'modifyTVar''
modifyTVarM' :: MonadSTM m => TVar a -> (a -> m a) -> m ()
modifyTVarM' tvar f = readTVar tvar >>= f >>= \x -> writeTVar tvar $! x

-- | Lifted version of 'STM.swapTVar'
swapTVar :: MonadSTM m => TVar a -> a -> m a
swapTVar tvar = liftSTM . STM.swapTVar tvar

-- TMVar

-- | Lifted version of 'STM.newTMVar'
newTMVar :: MonadSTM m => a -> m (TMVar a)
newTMVar = liftSTM . STM.newTMVar

-- | Lifted version of 'STM.newEmptyTMVar'
newEmptyTMVar :: MonadSTM m => m (TMVar a)
newEmptyTMVar = liftSTM STM.newEmptyTMVar

-- | Lifted version of 'STM.takeTMVar'
takeTMVar :: MonadSTM m => TMVar a -> m a
takeTMVar = liftSTM . STM.takeTMVar

-- | Lifted version of 'STM.putTMVar'
putTMVar :: MonadSTM m => TMVar a -> a -> m ()
putTMVar tmvar = liftSTM . STM.putTMVar tmvar

-- | Lifted version of 'STM.readTMVar'
readTMVar :: MonadSTM m => TMVar a -> m a
readTMVar = liftSTM . STM.readTMVar

-- | Lifted version of 'STM.tryReadTMVar'
tryReadTMVar :: MonadSTM m => TMVar a -> m (Maybe a)
tryReadTMVar = liftSTM . STM.tryReadTMVar

-- | Lifted version of 'STM.swapTMVar'
swapTMVar :: MonadSTM m => TMVar a -> a -> m a
swapTMVar tmvar = liftSTM . STM.swapTMVar tmvar

-- | Lifted version of 'STM.tryTakeTMVar'
tryTakeTMVar :: MonadSTM m => TMVar a -> m (Maybe a)
tryTakeTMVar = liftSTM . STM.tryTakeTMVar

-- | Lifted version of 'STM.tryPutTMVar'
tryPutTMVar :: MonadSTM m => TMVar a -> a -> m Bool
tryPutTMVar tvar = liftSTM . STM.tryPutTMVar tvar

-- | Lifted version of 'STM.isEmptyTMVar'
isEmptyTMVar :: MonadSTM m => TMVar a -> m Bool
isEmptyTMVar = liftSTM . STM.isEmptyTMVar

-- TChan

-- | Lifted version of 'STM.newTChan'
newTChan :: MonadSTM m => m (TChan a)
newTChan = liftSTM STM.newTChan

-- | Lifted version of 'STM.newBroadcastTChan'
newBroadcastTChan :: MonadSTM m => m (TChan a)
newBroadcastTChan = liftSTM STM.newBroadcastTChan

-- | Lifted version of 'STM.dupTChan'
dupTChan :: MonadSTM m => TChan a -> m (TChan a)
dupTChan = liftSTM . STM.dupTChan

-- | Lifted version of 'STM.cloneTChan'
cloneTChan :: MonadSTM m => TChan a -> m (TChan a)
cloneTChan = liftSTM . STM.cloneTChan

-- | Lifted version of 'STM.readTChan'
readTChan :: MonadSTM m => TChan a -> m a
readTChan = liftSTM . STM.readTChan

-- | Lifted version of 'STM.tryReadTChan'
tryReadTChan :: MonadSTM m => TChan a -> m (Maybe a)
tryReadTChan = liftSTM . STM.tryReadTChan

-- | Lifted version of 'STM.peekTChan'
peekTChan :: MonadSTM m => TChan a -> m a
peekTChan = liftSTM . STM.peekTChan

-- | Lifted version of 'STM.tryPeekTChan'
tryPeekTChan :: MonadSTM m => TChan a -> m (Maybe a)
tryPeekTChan = liftSTM . STM.tryPeekTChan

-- | Lifted version of 'STM.writeTChan'
writeTChan :: MonadSTM m => TChan a -> a -> m ()
writeTChan tchan = liftSTM . STM.writeTChan tchan

-- | Lifted version of 'STM.unGetTChan'
unGetTChan :: MonadSTM m => TChan a -> a -> m ()
unGetTChan tchan = liftSTM . STM.unGetTChan tchan

-- | Lifted version of 'STM.isEmptyTChan'
isEmptyTChan :: MonadSTM m => TChan a ->  m Bool
isEmptyTChan = liftSTM . STM.isEmptyTChan

-- TQueue

-- | Lifted version of 'STM.newTQueue'
newTQueue :: MonadSTM m => m (TQueue a)
newTQueue = liftSTM STM.newTQueue

-- | Lifted version of 'STM.readTQueue'
readTQueue :: MonadSTM m => TQueue a -> m a
readTQueue = liftSTM . STM.readTQueue

-- | Lifted version of 'STM.tryReadTQueue'
tryReadTQueue :: MonadSTM m => TQueue a -> m (Maybe a)
tryReadTQueue = liftSTM . STM.tryReadTQueue

-- | Lifted version of 'STM.peekTQueue'
peekTQueue :: MonadSTM m => TQueue a -> m a
peekTQueue = liftSTM . STM.peekTQueue

-- | Lifted version of 'STM.tryPeekTQueue'
tryPeekTQueue :: MonadSTM m => TQueue a -> m (Maybe a)
tryPeekTQueue = liftSTM . STM.tryPeekTQueue

-- | Lifted version of 'STM.writeTQueue'
writeTQueue :: MonadSTM m => TQueue a -> a -> m ()
writeTQueue tchan = liftSTM . STM.writeTQueue tchan

-- | Lifted version of 'STM.unGetTQueue'
unGetTQueue :: MonadSTM m => TQueue a -> a -> m ()
unGetTQueue tchan = liftSTM . STM.unGetTQueue tchan

-- | Lifted version of 'STM.isEmptyTQueue'
isEmptyTQueue :: MonadSTM m => TQueue a ->  m Bool
isEmptyTQueue = liftSTM . STM.isEmptyTQueue

-- TBQueue

-- | Lifted version of 'STM.newTBQueue'
newTBQueue :: MonadSTM m => Natural -> m (TBQueue a)
newTBQueue = liftSTM . STM.newTBQueue

-- | Lifted version of 'STM.readTBQueue'
readTBQueue :: MonadSTM m => TBQueue a -> m a
readTBQueue = liftSTM . STM.readTBQueue

-- | Lifted version of 'STM.tryReadTBQueue'
tryReadTBQueue :: MonadSTM m => TBQueue a -> m (Maybe a)
tryReadTBQueue = liftSTM . STM.tryReadTBQueue

-- | Lifted version of 'STM.peekTBQueue'
peekTBQueue :: MonadSTM m => TBQueue a -> m a
peekTBQueue = liftSTM . STM.peekTBQueue

-- | Lifted version of 'STM.tryPeekTBQueue'
tryPeekTBQueue :: MonadSTM m => TBQueue a -> m (Maybe a)
tryPeekTBQueue = liftSTM . STM.tryPeekTBQueue

-- | Lifted version of 'STM.writeTBQueue'
writeTBQueue :: MonadSTM m => TBQueue a -> a -> m ()
writeTBQueue tchan = liftSTM . STM.writeTBQueue tchan

-- | Lifted version of 'STM.unGetTBQueue'
unGetTBQueue :: MonadSTM m => TBQueue a -> a -> m ()
unGetTBQueue tchan = liftSTM . STM.unGetTBQueue tchan

-- | Lifted version of 'STM.isEmptyTBQueue'
isEmptyTBQueue :: MonadSTM m => TBQueue a ->  m Bool
isEmptyTBQueue = liftSTM . STM.isEmptyTBQueue

-- | Lifted version of 'STM.isFullTBQueue'
isFullTBQueue :: MonadSTM m => TBQueue a ->  m Bool
isFullTBQueue = liftSTM . STM.isFullTBQueue
