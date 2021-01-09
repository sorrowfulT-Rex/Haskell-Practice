module NumStruc where

import           Control.Monad.Trans.State
import           Data.Maybe

import           Queue

primeList :: [Integer]
primeList = filterAcc isPrimeAcc [2..] emptyQueue

ulamList :: [Integer]
ulamList = 1 : 2 : filterAcc isUlamAcc [3..] (makeQueue [1, 2])

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
  us <- get
  pop
  isUlamAcc' n 0 us
  where
    isUlamAcc' _ 2 _
      = return False
    isUlamAcc' n c us
      | empty us  = return $ c == 1
      | otherwise = do
        let ((Just u), us') = runState pop us
        if u > n `div` 2
          then return $ c == 1
          else test u us' c
        where
          test u us c = do
            u' <- pop
            if isNothing u'
              then do
                put us
                pop
                isUlamAcc' n c us
              else test' (u + fromJust (u'))
              where
                test' sum 
                  | sum < n   = test u us c
                  | sum == n  = test u us (c + 1)
                  | otherwise = do
                    put us
                    pop
                    isUlamAcc' n c us
