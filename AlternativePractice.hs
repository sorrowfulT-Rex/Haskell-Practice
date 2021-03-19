import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.Maybe

digit :: Int -> String -> Maybe (Char, String)
digit d s = do
  d' : s' <- return s
  guard (head (show d) == d')
  return (d', s')

char :: Char -> String -> Maybe (Char, String)
char c s = do
  c' : s' <- return s
  guard (c == c')
  return (c, s')

hexChar :: String -> Maybe (Char, String)
hexChar s 
  = mplus numCheck alfCheck
  where
    numCheck = msum (ap (map digit [0..9]) $ return s)
    alfCheck = msum (ap (map char "abcdef") $ return s)

hexCheck :: String -> (String, String)
hexCheck ""
  = ("", "")
hexCheck s
  | isNothing mhc = ("", s)
  | otherwise     = let (x, y) = hexCheck (tail s) in (hc : x, y)
  where
    mhc = hexChar s
    hc  = fst $ fromJust mhc

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog = join ((. log) . ($>) . guard . (> 0))
