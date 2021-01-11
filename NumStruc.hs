import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Maybe
import           Prelude hiding (fst, snd)
import           System.IO
import           System.IO.Unsafe

import           Queue

-- Read the primes already calculated
primeListR :: IO [Integer]
primeListR = do
  h   <- openFile "primes.txt" ReadWriteMode
  raw <- hGetContents h
  return $ map read (words raw)

-- Generate an inifite list of primes
primeList :: IO [Integer]
primeList = do
  -- Get current primes
  list <- primeListR
  -- Infinite list of new primes since current primes
  let list' = execState primeListS list
  -- Write these new primes into the file
  rst <- primeListW list' False
  return $ list ++ rst
  where
    primeListS = do
      curList <- get
      let queue = makeQueue (reverse curList)
      let last  = evalState popFrontS queue
      let start = maybe 2 succ last
      -- Main function to generate new primes is isPrimeAcc, using my deque
      put $ filterAcc isPrimeAcc [start..] queue
    primeListW (a : as) b
      | b         = do
        appendFile "primes.txt" (' ' : show a)
        rst <- unsafeInterleaveIO $ primeListW as True
        return $ a : rst
      | otherwise = unsafeInterleaveIO $ primeListW (a : as) True

-- This has similar structure as above, but I'm writing it again to strengthen my memory
-- Read the ulam numbers already calculated
ulamListR :: IO [Integer]
ulamListR = do
  h   <- openFile "ulams.txt" ReadWriteMode
  raw <- hGetContents h
  return $ map read (words raw)

-- Generate an infiite list of ulam numbers
ulamList :: IO [Integer]
ulamList = do
  list <- ulamListR
  let list' = execState ulamListS list
  rst <- ulamListW list' False
  return $ 1 : 2 : list ++ rst
  where
    ulamListS = do
      curList <- get
      let queue = makeQueue (reverse (1 : 2 : curList))
      let last  = evalState popFrontS queue
      let start = maybe 2 succ last
      put $ filterAcc isUlamAcc [start..] queue
    ulamListW (a : as) b 
      | b         = do
        appendFile "ulams.txt" (' ' : show a)
        rst <- unsafeInterleaveIO $ ulamListW as True
        return $ a : rst
      | otherwise = unsafeInterleaveIO $ ulamListW (a : as) True

filterAcc :: (a -> QueueS a Bool) -> [a] -> Queue a -> [a]
filterAcc _ [] _
  = []
filterAcc f (a : as) q 
  | passed    = a : filterAcc f as q'
  | otherwise = filterAcc f as q
  where
    passed = evalState (f a) q
    q'     = execState (pushS a) q

isPrimeAcc :: Integer -> QueueS Integer Bool
isPrimeAcc n = do
  this <- popS
  if isNothing this
    then return True
    else do
      test $ fromJust this
      where
        test p 
          | p ^ 2 > n     = return True
          | n `mod`p == 0 = return False
          | otherwise     = isPrimeAcc n

isUlamAcc :: Integer -> QueueS Integer Bool
isUlamAcc n = do
  fst <- popFrontS
  lst <- popS
  isUlamAcc' 0 fst lst
  where
    isUlamAcc' 2 _ _
      = return False
    isUlamAcc' c fst lst = do
      let sum = liftA2 (+) fst lst
      if isNothing sum
        then return $ c == 1
        else test (fromJust sum)
        where
          test sum
            | sum > n   = do
              fst' <- popFrontS
              isUlamAcc' c fst' lst
            | sum < n   = do
              lst' <- popS
              isUlamAcc' c fst lst'
            | otherwise = do
              fst' <- popFrontS
              lst' <- popS
              isUlamAcc' (c + 1) fst' lst'
