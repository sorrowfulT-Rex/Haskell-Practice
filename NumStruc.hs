module NumStruc where

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Maybe
import           Prelude hiding (fst, snd)

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
