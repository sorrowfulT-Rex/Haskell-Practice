module Queue where

import           Control.Monad.Trans.State
import           Data.Maybe

data Queue a = Queue Int [a] Int [a]

instance Show a => Show (Queue a) where
  show (Queue _ [] _ [])
    = "Empty Queue"
  show q
    = "[..." ++ show (fromJust $ evalState peek q) ++ "]"

type QueueS a = State (Queue a)

emptyQueue :: Queue a
emptyQueue
  = Queue 0 [] 0 []

-- Initialise a queue
makeQueue :: [a] -> Queue a
makeQueue as
  = Queue 0 [] (length as) as

-- Check if a queue is empty
empty :: Queue a -> Bool
empty (Queue _ [] _ [])
  = True
empty _ 
  = False

-- Check if we need to reverse the inbox list and fill it to the outbox list
check :: Queue a -> Queue a
check q@(Queue inl ins ol os)
  | inl <= ol = q
  | otherwise = Queue 0 [] (inl + ol) (os ++ reverse ins)

-- Add element to the front of the queue
push :: a -> QueueS a ()
push e 
  = state $ \q -> ((), push' e q)
  where
    push' e (Queue inl ins ol os) 
      = check (Queue (inl + 1) (e : ins) ol os)

-- Add element to the end of the queue
pushEnd :: a -> QueueS a ()
pushEnd e 
  = state $ \q -> ((), pushEnd' e q)
  where
    pushEnd' e (Queue inl ins ol os) 
      = Queue inl ins (ol + 1) (e : os)

-- Retrieve element from the end of the queue
pop :: QueueS a (Maybe a)
pop 
  = state $ \q -> pop' q
  where
    pop' q@(Queue _ _ [])
      = (Nothing, q)
    pop' (Queue inl ins ol (o : os))
      = (Just o, check (Queue inl ins (ol - 1) os))

-- Look at the ending element of the queue without changing the queue
peek :: QueueS a (Maybe a)
peek = do
  e <- pop
  if isNothing e
    then return e
    else do
      pushEnd (fromJust e)
      return e

-- Length of the queue
len :: Queue a -> Int
len (Queue inl _ ol _) 
  = inl + ol
