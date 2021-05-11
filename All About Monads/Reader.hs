import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity

-- Implementation of ReaderT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader = (runIdentity .) . runReaderT

instance Monad m => Functor (ReaderT r m) where
  fmap = liftM

instance Monad m => Applicative (ReaderT r m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ReaderT r m) where
  return = ReaderT . const . return
  ReaderT r >>= f
    = ReaderT $ \a -> r a >>= \a' -> runReaderT (f a') a

instance Monad m => MonadFail (ReaderT e m) where
  fail = error

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

-- Return the environment
ask :: Monad m => ReaderT r m r
ask = ReaderT return

-- Return the environment modified by the given function
asks :: Monad m => (r -> a) -> ReaderT r m a
asks = flip fmap ask

-- Return a new reader that modifies the environment with the given function
local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT

-- Constructor of ReaderT; same as asks
reader :: Monad m => (r -> a) -> ReaderT r m a
reader = asks

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader = fmap

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f (ReaderT r) = ReaderT $ f . r

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader = withReaderT

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f (ReaderT r) 
  = ReaderT $ r . f

-- Example

tom :: Monad m => ReaderT String m String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Monad m => ReaderT String m String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Monad m => ReaderT String m String
tomAndJerry = do
    t <- tom
    j <- local (++ " You know, ") jerry
    return (t ++ "\n" ++ j)

runJerryRun :: IO ()
runJerryRun = do
  putStrLn "Starting runJerryRun..."
  str <- runReaderT (lift (putStrLn "Running...") >> tomAndJerry) ""
  putStrLn str
  putStrLn "That's it!"
