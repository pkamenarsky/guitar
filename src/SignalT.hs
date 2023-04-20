{-# LANGUAGE DeriveFunctor #-}

module SignalT where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

data SignalT m a = SignalT (m (a, SignalT m a))
  deriving Functor

instance Monad m => Applicative (SignalT m) where
  pure a = SignalT $ pure (a, pure a)
  SignalT f <*> SignalT a = SignalT $ do
    (f', nf) <- f
    (a', na) <- a
    pure (f' a', nf <*> na)

instance Monad m => Monad (SignalT m) where
  SignalT m >>= f = SignalT $ do
    (a, n) <- m
    case f a of
      SignalT m' -> do
        (b, n') <- m'
        pure (b, n')

instance MonadTrans SignalT where
  lift f = SignalT $ do
    a <- f
    pure (a, lift f)

instance MonadIO m => MonadIO (SignalT m) where
  liftIO f = SignalT $ do
    a <- liftIO f
    pure (a, liftIO f)

recur :: Applicative m => a -> SignalT m a -> SignalT m a
recur a next = SignalT $ pure (a, next)
