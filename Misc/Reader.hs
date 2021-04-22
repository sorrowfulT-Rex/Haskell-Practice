import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad

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

g :: Int -> State (Integer, Integer) Integer
g n = forM_ [1..n] (const $ get >>= \(x, y) -> put (y, x + y)) >> fst <$> get
