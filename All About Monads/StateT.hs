import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Text as T
import Data.Functor.Identity

-- TODO: Implement your own State Monad
newtype StateT e m a = StateT { runStateT :: e -> m (a, e) }

type State r = StateT r Identity

instance Monad m => Functor (StateT e m) where
  fmap = liftM

instance Monad m => Applicative (StateT e m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (StateT e m) where
  return = StateT . (return .) . (,)
  StateT s >>= f = StateT $ \e -> do
    (a, e') <- s e 
    runStateT (f a) e'

instance Monad m => MonadFail (StateT e m) where
  fail = error

instance MonadTrans (StateT e) where
  lift m = StateT $ \e -> m >>= \a -> return (a, e)

evalState :: State e a -> e -> a
evalState = (runIdentity .) . evalStateT

evalStateT :: Monad m => StateT e m a -> e -> m a
evalStateT = (fmap fst .) . runStateT

execState :: State e a -> e -> e
execState = (runIdentity .) . execStateT

execStateT :: Monad m => StateT e m a -> e -> m e
execStateT = (fmap snd .) . runStateT

get :: Monad m => StateT s m s
get = StateT $ return . join (,)

put :: Monad m => s -> StateT s m ()
put s = StateT $ const $ return ((), s)


-- Simple Text Editor. Start from an empty text, then apply a number of queries.
-- 1 arg: append arg to the end of the text;
-- 2 arg: remove the last arg elements from the text;
-- 3 arg: print the character at index (arg - 1);
-- 4    : undo last operation.

main :: IO ()
main = do
  numQuery <- readLn :: IO Int
  evalStateT (forM_ [1..numQuery] $ const $ lift getLine >>= operate) [T.empty] 

operate :: String -> StateT [Text] IO ()
operate ('1' : ' ' : args) = do
  tz@(t : ts) <- get
  put $ T.concat [t, pack args] : tz
operate ('2' : ' ' : args) = do
  tz@(t : ts) <- get
  put $ T.take (T.length t - read args) t : tz
operate ('3' : ' ' : args) = do
  (t : ts) <- get
  lift $ putChar (t `index` (read args - 1)) >> putChar '\n'
operate "4" = do
  (t : ts) <- get
  put ts
