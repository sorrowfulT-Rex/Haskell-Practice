import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe
import           Data.Set hiding (filter)

skewCollatz :: Int -> Set Int
skewCollatz n
  = skew' (singleton n) n
  where
    skew' t n
      | cycled    = t
      | otherwise = skew' t' n'
      where
        t'     = insert n' t
        cycled = member n' t
        n'     = if n `mod` 2 == 0 then n `div` 2 else 3 * n - 1

minCol :: Int -> Int
minCol
  = minimum . skewCollatz

main :: IO ()
main = forM_ [1..1000] (\s -> do
  putStr $ show s
  putStr ": "
  print $ minCol s
  )

minList :: Int -> [(Int, Int)]
minList n
  = foldl' min' [] $ fromList [1..n]
  where
    min' dict n
      | isNothing search = (n, 1) : dict
      | otherwise        = (minned, fromJust search + 1) : dict'
      where
        minned = minCol n
        search = (lookup minned dict)
        dict'  = filter ((/= minned) . fst) dict
