-- import Control.Monad.Trans.Reader
import Control.Applicative
import Control.Monad

-- TODO: Implement your own Read Monad with ask and local
newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
  fmap = liftM

instance Applicative (Reader e) where
  pure  = return
  (<*>) = ap

instance Monad (Reader e) where
  return = Reader . const
  Reader r >>= f
    = Reader $ \a -> runReader (f (r a)) a

ask :: Reader e e
ask = Reader id

local :: (e -> e) -> Reader e a -> Reader e a
local f (Reader r)
  = Reader $ r . f

tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- local (++ " You know,") jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = runReader tomAndJerry "Who is this?"

f :: Int -> Reader (Integer, Integer) Integer
f n = foldl (\r _ -> local (\(x, y) -> (y, x + y)) r) (fst <$> ask) [1..n]
