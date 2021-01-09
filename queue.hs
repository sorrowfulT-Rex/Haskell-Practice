module Queue where

-- Making a deque with amortised O(1) accesses

import           Control.Monad.Trans.State
import           Data.Maybe

data Queue a = Queue Int [a] Int [a]
  -- deriving Show

instance Show a => Show (Queue a) where
  show (Queue _ [] _ [])
    = "[]"
  show (Queue _ [a] _ [])
    = '[' : show a ++ "]"
  show (Queue _ [] _ [a])
    = '[' : show a ++ "]"
  show q
    = '[' : show (fromJust $ evalState peekFront q) ++ "..." ++ show (fromJust $ evalState peek q) ++ "]"

type QueueS a = State (Queue a)

emptyQueue :: Queue a
emptyQueue
  = Queue 0 [] 0 []

-- Initialise a queue (a deque acutally)
makeQueue :: [a] -> Queue a
makeQueue as
  = Queue len fr len' re
  where
    (q, r)    = quotRem (length as) 2
    len       = q
    len'      = q + r
    (fr, re') = splitAt len as
    re        = reverse re'

-- Check if the queue is empty
empty :: Queue a -> Bool
empty (Queue _ [] _ [])
  = True
empty _ 
  = False

-- Check if we need to reverse the inbox list and fill it to the outbox list
check :: Queue a -> Queue a
check q@(Queue inl ins ol os)
  | 2 * inl < ol = Queue (inl + len) (ins ++ reverse oR) (ol - len) oF
  | 2 * ol < inl = Queue (inl - len) inF (ol + len) (os ++ reverse inR)
  | otherwise    = q
  where
    (len, re)  = quotRem (abs (ol - inl)) 2
    (inF, inR) = splitAt (inl - len) ins
    (oF, oR)   = splitAt (ol - len) os

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
      = check $ Queue inl ins (ol + 1) (e : os)

-- Retrieve element from the end of the queue
pop :: QueueS a (Maybe a)
pop 
  = state $ \q -> pop' q
  where
    pop' q@(Queue _ [] _ [])
      = (Nothing, q)
    pop' (Queue _ [a] _ [])
      = (Just a, emptyQueue)
    pop' (Queue _ [] _ [a])
      = (Just a, emptyQueue)
    pop' (Queue inl ins ol (o : os))
      = (Just o, check (Queue inl ins (ol - 1) os))

-- Retrieve element from the front of the queue
popFront :: QueueS a (Maybe a)
popFront
  = state $ \q -> popFront' q 
  where
    popFront' q@(Queue _ [] _ [])
      = (Nothing, q)
    popFront' (Queue _ [a] _ [])
      = (Just a, emptyQueue)
    popFront' (Queue _ [] _ [a])
      = (Just a, emptyQueue)
    popFront' (Queue inl (i : ins) ol os)
      = (Just i, check (Queue (inl - 1) ins ol os))

-- Look at the last element of the queue without changing the queue
peek :: QueueS a (Maybe a)
peek = do
  e <- pop
  if isNothing e
    then return e
    else do
      pushEnd (fromJust e)
      return e

-- Look at the first element of the queue without changing the queue
peekFront :: QueueS a (Maybe a)
peekFront = do
  e <- popFront
  if isNothing e
    then return e
    else do
      push (fromJust e)
      return e

-- Length of the queue
len :: Queue a -> Int
len (Queue inl _ ol _) 
  = inl + ol
