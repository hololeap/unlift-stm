# unlift-stm

### (un)lifted classes and functions for the STM monad

Lifted and unlifted classes and functions for the STM monad. To get
started, just import `Control.Concurrent.STM.Unlift`. This is meant to
be a drop-in replacement for [`Control.Concurrent.STM`][STM].

This package provides functionality for the STM monad, which is highly
analagous to the functionalty for the IO monad found
in `Control.Monad.IO.Class`, `Control.Monad.IO.Unlift`, and
`UnliftIO.STM`:

- `MonadSTM` is a copy of [`MonadIO`][MonadIO] modified for STM
- `MonadUnliftSTM` is a copy of [`MonadUnliftIO`][MonadUnliftIO] modified for
  STM
- Some functions found in `Control.Concurrent.STM.Unlift` are re-exported
  directly from [`UnliftIO.STM`][UnliftIO-STM]

[STM]: https://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM.html
[MonadIO]: https://hackage.haskell.org/package/base/docs/Control-Monad-IO-Class.html#t:MonadIO
[MonadUnliftIO]: https://hackage.haskell.org/package/unliftio-core/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO
[UnliftIO-STM]: https://hackage.haskell.org/package/unliftio/docs/UnliftIO-STM.html
