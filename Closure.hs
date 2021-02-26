module Closure where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable (foldl')
import           Prelude hiding (filter)

import           Data.Set hiding (foldl', drop)

-- Functional Dependencies
data FD a = FD (Set (Set a, Set a))

exampleFD :: FD Char
exampleFD = toFD [("AB", "DEH"), ("BEF", "A"), ("FGH", "C"), ("D", "EG"), ("EG", "BF"), ("F", "BH")]

runExample :: IO ()
runExample = stepByStepSolution exampleFD

stepByStepSolution :: (Ord a, Show a) => FD a -> IO ()
stepByStepSolution fd = do
  putStrLn "Original FDs:"
  print fd
  let fd1 = step1 fd
  putStrLn "Step 1: "
  print fd1
  let fd2 = step2 fd1
  putStrLn "Step 2: "
  print fd2
  let fd3 = step3 fd2
  putStrLn "Step 3: "
  print fd3
  let fd4 = step4 fd3
  putStrLn "Combine RHS: "
  print fd4

instance Show a => Show (FD a) where
  show (FD fd)
    = drop 2 $ foldl' show' "" list ++ "."
    where
      list = toList fd
      show' str (k, v)
        =  str ++ "; " ++ show (toList k) ++ ": " ++ show (toList v)

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll key
  = foldl' look []
  where
    look res (k, v)
      | k == key = v : res
      | otherwise = res

toFD :: Ord a => [([a], [a])] -> FD a
toFD list
  = FD $ foldl' toFD' empty list
  where
    toFD' fd (ks, vs)
      = insert (fromList ks, (fromList $ join $ lookupAll ks list)) fd

closure :: Ord a => Set a -> FD a -> Set a
closure set fds@(FD fd)
  | size goOnce == size set = goOnce
  | otherwise               = closure goOnce fds
  where
    goOnce = execState (forM_ fd closure') set
    closure' (ks, vs) = if intersection ks set == ks
      then get >>= put . union vs
      else return ()

removeKey :: Ord a => Set a -> FD a -> FD a
removeKey key (FD fd)
  = FD $ filter ((key /=) . fst) fd

step1 :: Ord a => FD a -> FD a
step1 (FD fd)
  = FD $ execState (forM_ fd step1') empty
  where
    step1' (ks, vs) 
      = forM_ vs $ \s -> get >>= put . insert (ks, singleton s) 
      
step2 :: Ord a => FD a -> FD a
step2 fds@(FD fd)
  = FD $ execState (forM_ fd step2') fd
  where
    step2' (ks, vs) 
      = forM_ ks $ \k -> do
        let fd' = removeKey ks fds
        let ks' = delete k ks
        if member k (closure ks' fd')
          then do
            fd <- get
            put $ insert (ks', vs) $ delete (ks, vs) fd
            step2' (ks', vs)
          else return ()

step3 :: Ord a => FD a -> FD a
step3 (FD fd)
  = FD $ filter step3' fd
  where
    step3' (ks, vs)
      = intersection vs (closure ks (FD $ delete (ks, vs) fd)) /= vs

step4 :: Ord a => FD a -> FD a
step4 (FD fd)
  = FD $ execState (forM_ fd step4') fd
  where
    step4' (ks, vs) = do
      set <- get
      if notMember (ks, vs) set
        then return ()
        else do
          let entries = filter ((ks ==) . fst) fd
          forM_ entries $ \s -> get >>= put . delete s
          let unioned = execState (forM_ entries $ \(ks, vs) -> do
              set <- get
              put $ union vs set
              ) empty
          get >>= put . insert (ks, unioned)
