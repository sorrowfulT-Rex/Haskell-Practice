import           Control.Applicative
import           Data.Maybe

import           Queue
import           NumStruc

-- Read the primes already calculated
primeListR :: IO [Integer]
primeListR = numRead "primes.txt" []

-- Read the ulam numbers already calculated
ulamListR :: IO [Integer]
ulamListR = numRead "ulams.txt" [1, 2]

-- Generate an inifite list of primes
primeList :: IO [Integer]
primeList 
 = numListIO "primes.txt" [] 2 isPrimeAcc

-- Generate an infiite list of ulam numbers
ulamList :: IO [Integer]
ulamList
 = numListIO "ulams.txt" [1, 2] 3 isUlamAcc

-- Accumulation filter for primes
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

-- Accumulation filter for ulam numbers
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
