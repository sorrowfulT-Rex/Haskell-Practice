import Control.Monad ( forM_ )
import Control.Monad.Trans.State ( evalStateT, get, put, StateT )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Data.Text as T
    ( concat, index, length, pack, take, empty, Text )

-- Simple Text Editor. Start from an empty text, then apply a number of queries.
-- 1 arg: append arg to the end of the text;
-- 2 arg: remove the last arg elements from the text;
-- 3 arg: print the character at index (arg - 1);
-- 4    : undo last operation.

main :: IO ()
main = do
  numQuery <- readLn :: IO Int
  evalStateT (forM_ [1..numQuery] $ const $ do
    query <- lift getLine
    operate query
    ) [T.empty] 

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
