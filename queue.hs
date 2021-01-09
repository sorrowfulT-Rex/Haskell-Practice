module Queue where

import           Control.Monad.Trans.State
import           Data.Maybe

data Queue a = Queue [a] [a]

instance Show a => Show (Queue a) where
  show (Queue [] [])
    = "Empty Queue"
  show q
    = "[..." ++ show (fromJust $ evalState peek q) ++ "]"

type QueueS a = State (Queue a)

emptyQueue :: Queue a
emptyQueue
  = Queue [] []

-- Initialise a queue
makeQueue :: [a] -> Queue a
makeQueue 
  = Queue []

-- Check if a queue is empty
empty :: Queue a -> Bool
empty (Queue [] [])
  = True
empty _ 
  = False

-- Add element to the front of the queue
push :: a -> QueueS a ()
push e 
  = state $ \q -> ((), push' e q)
  where
    push' e (Queue ins os) 
      = Queue (e : ins) os

-- Add element to the end of the queue
pushEnd :: a -> QueueS a ()
pushEnd e 
  = state $ \q -> ((), pushEnd' e q)
  where
    pushEnd' e (Queue ins os) 
      = Queue ins (e : os)

-- Retrieve element from the end of the queue
pop :: QueueS a (Maybe a)
pop 
  = state $ \q -> pop' q
  where
    pop' q@(Queue [] [])
      = (Nothing, q)
    pop' (Queue ins [])
      = pop' $ Queue [] (reverse ins)
    pop' (Queue ins (o : os))
      = (Just o, Queue ins os)

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
len (Queue ins os) 
  = length ins + length os
