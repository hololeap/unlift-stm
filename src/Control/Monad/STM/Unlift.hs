{-|
Module      : Control.Monad.STM.Unlift
Licesnse    : BSD-2
Stability   : experimental

A typeclass which abstracts over monads that can be "unlifted" back down to
'STM'. (This is essentially a copy of "Control.Monad.IO.Unlift", modified for
the 'STM' monad.)

In a manner that is analagous to 'MonadUnliftIO', instances of this typeclass
are essentially limited to:

    * The 'STM' monad itself
    * Monad transformer stacks based on 'ReaderT' or 'IdentityT', with
      'STM' at the "bottom".
-}

{-# Language RankNTypes #-}

module Control.Monad.STM.Unlift
    ( MonadUnliftSTM(..)
    , UnliftSTM(..)
    , askRunInSTM
    , withUnliftSTM
    , toSTM
    ) where

import Control.Concurrent.STM (STM)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.STM.Class
import Data.Functor

-- | Analagous to 'MonadUnliftIO'. (It is an abstraction for monads that can
--   be "unlifted" back down to the 'STM' monad.)
class MonadSTM m => MonadUnliftSTM m where
    askUnliftSTM :: m (UnliftSTM m)
    askUnliftSTM = withRunInSTM $ \f -> pure (UnliftSTM f)

    withRunInSTM :: ((forall a. m a -> STM a) -> STM b) -> m b
    withRunInSTM f = askUnliftSTM >>= \u -> liftSTM (f (unliftSTM u))

    {-# MINIMAL askUnliftSTM | withRunInSTM #-}


instance MonadUnliftSTM STM where
    askUnliftSTM = pure (UnliftSTM id)
    withRunInSTM f = f id

instance MonadUnliftSTM m => MonadUnliftSTM (ReaderT r m) where
    askUnliftSTM = ReaderT $ \r -> askUnliftSTM <&> \u ->
        UnliftSTM $ unliftSTM u . flip runReaderT r

    withRunInSTM f = ReaderT $ \r -> withRunInSTM $ \run ->
        f (run . flip runReaderT r) 

instance MonadUnliftSTM m => MonadUnliftSTM (IdentityT m) where
    askUnliftSTM = IdentityT $ askUnliftSTM <&> \u ->
        UnliftSTM $ unliftSTM u . runIdentityT

    withRunInSTM f = IdentityT $ withRunInSTM $ \run -> f (run . runIdentityT)

-- | Analagous to 'UnliftIO'
newtype UnliftSTM m = UnliftSTM { unliftSTM :: forall a. m a -> STM a }

-- | Analagous to 'askRunInIO'
askRunInSTM :: MonadUnliftSTM m => m (m a -> STM a)
askRunInSTM = withRunInSTM (\run -> pure (\ma -> run ma))

-- | Analagous to 'withUnliftIO'
withUnliftSTM :: MonadUnliftSTM m => (UnliftSTM m -> STM a) -> m a
withUnliftSTM inner = askUnliftSTM >>= liftSTM . inner

-- | Analagous to 'toIO'
toSTM :: MonadUnliftSTM m => m a -> m (STM a)
toSTM m = withRunInSTM $ \run -> pure (run m)
