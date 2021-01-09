module NumStruc where

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Maybe

import           Queue

primeList :: [Integer]
primeList = filterAcc isPrimeAcc [2..] emptyQueue

ulamList :: [Integer]
ulamList = 1 : 2 : filterAcc isUlamAcc [3..] (makeQueue [2, 1])

filterAcc :: (a -> QueueS a Bool) -> [a] -> Queue a -> [a]
filterAcc _ [] _
  = []
filterAcc f (a : as) q 
  | passed    = a : filterAcc f as q'
  | otherwise = filterAcc f as q
  where
    passed = evalState (f a) q
    q'     = execState (push a) q

isPrimeAcc :: Integer -> QueueS Integer Bool
isPrimeAcc n = do
  this <- pop
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
  fst <- popFront
  lst <- pop
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
              fst' <- popFront
              isUlamAcc' c fst' lst
            | sum < n   = do
              lst' <- pop
              isUlamAcc' c fst lst'
            | otherwise = do
              fst' <- popFront
              lst' <- pop
              isUlamAcc' (c + 1) fst' lst'
