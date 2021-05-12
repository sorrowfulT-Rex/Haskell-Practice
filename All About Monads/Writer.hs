{-# LANGUAGE TupleSections #-}
import Control.Monad
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Data.Functor.Identity

import qualified Data.DList as DL

-- TODO: Implement your own Writer Monad
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

type Writer w a = WriterT w Identity a

instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap = liftM

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure  = return
  (<*>) = ap

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return = WriterT . return . (, mempty)
  WriterT s >>= f = WriterT $ do
    (a, w)  <- s
    (a, w') <- runWriterT (f a)
    return (a, w `mappend` w')

instance (Monoid w, Monad m) => MonadFail (WriterT w m) where
  fail = error

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT $ m >>= \a -> return (a, mempty)

censor :: (Monoid w, Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
censor f s = pass $ s >>= \a -> return (a, f)

execWriter :: Monoid w => Writer w a -> w
execWriter = snd . runWriter

execWriterT :: (Monoid w, Monad m) => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT

listen :: (Monoid w, Monad m) => WriterT w m a -> WriterT w m (a, w)
listen (WriterT s) = WriterT $ s >>= \(a, w) -> return ((a, w), w)

pass :: (Monoid w, Monad m) => WriterT w m (a, w -> w) -> WriterT w m a
pass (WriterT s) = WriterT $ s >>= \((a, f), w) -> return (a, f w)

tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = WriterT $ return ((), w)

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT


toBin :: Integer -> [Int]
toBin = reverse . DL.toList . execWriter . go
  where
    go i = do
      let (q, r) = quotRem i 2
      (_, w) <- listen $ do
        tell (DL.singleton $ fromIntegral r)
      when (q > 0) (go q)
