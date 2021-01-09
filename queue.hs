module Queue where

-- Making a deque with amortised O(1) accesses

import           Control.Monad.Trans.State
import           Data.Maybe

-- Has an inbox (the first half of elements) and an outbox (the second half)
-- Ideally, both halves should have similar number of elements
-- Note that the Queue constructor is strict; if you use InfQueue, make sure that it is indeed infinite
data Queue a = Queue Int ![a] Int ![a] | InfQueue [a] [a]
  -- deriving Show

instance Show a => Show (Queue a) where
  show (Queue _ [] _ [])
    = "[]"
  show (Queue _ [a] _ [])
    = '[' : show a ++ "]"
  show (Queue _ [] _ [a])
    = '[' : show a ++ "]"
  -- Showing the first and the last elements
  show q
    = '[' : show (fromJust $ evalState peekFront q) ++ "..." ++ show (fromJust $ evalState peek q) ++ "]"

instance Functor Queue where
  fmap f (Queue inl ins ol os)
    = Queue inl (map f ins) ol (map f os)

instance Applicative Queue where
  pure x
    = InfQueue (repeat x) (repeat x)
  (<*>) (Queue _ [] _ []) _
    = emptyQueue
  (<*>) _ (Queue _ [] _ [])
    = emptyQueue
  (<*>) fs xs
    = execState (push (f x)) (fr <*> xr)
    where
      (Just f, fr) = runState popFront fs
      (Just x, xr) = runState popFront xs

-- Convert deque to list
toList :: Queue a -> [a]
toList (Queue _ ins _ os)
  = ins ++ reverse os
toList (InfQueue ins _)
  = ins

-- Making a State instance for more convenient manipulations
type QueueS a = State (Queue a)

emptyQueue :: Queue a
emptyQueue
  = Queue 0 [] 0 []

-- Initialise a deque
makeQueue :: [a] -> Queue a
makeQueue as
  = Queue len fr len' re
  where
    (q, r)    = quotRem (length as) 2
    len       = q
    len'      = q + r
    (fr, re') = splitAt len as
    re        = reverse re'

-- Check if the deque is empty
empty :: Queue a -> Bool
empty (Queue _ [] _ [])
  = True
empty _
  = False

-- When either the inbox or the outbox is too small compared to the other, shuffle it to make them even
check :: Queue a -> Queue a
check q@(Queue inl ins ol os)
  | 2 * inl < ol = Queue (inl + len) (ins ++ reverse oR) (ol - len) oF
  | 2 * ol < inl = Queue (inl - len) inF (ol + len) (os ++ reverse inR)
  | otherwise    = q
  where
    (len, re)  = quotRem (abs (ol - inl)) 2
    (inF, inR) = splitAt (inl - len) ins
    (oF, oR)   = splitAt (ol - len) os
check infQ
  = infQ

-- Add element to the front of the deque
push :: a -> QueueS a ()
push e 
  = state $ \q -> ((), push' e q)
  where
    push' e (Queue inl ins ol os) 
      = check (Queue (inl + 1) (e : ins) ol os)
    push' e (InfQueue ins os)
      = InfQueue (e : ins) os

-- Add element to the end of the deque
pushEnd :: a -> QueueS a ()
pushEnd e 
  = state $ \q -> ((), pushEnd' e q)
  where
    pushEnd' e (Queue inl ins ol os) 
      = check $ Queue inl ins (ol + 1) (e : os)
    push' e (InfQueue ins os)
      = InfQueue ins (e : os)

-- Retrieve element from the end of the deque
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
    pop' (InfQueue ins (o : os))
      = (Just o, InfQueue ins os)
    pop' _
      = error "This InfQueue is not infinite!"

-- Retrieve element from the front of the deque
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
    popFront' (InfQueue (i : ins) os)
      = (Just i, InfQueue ins os)
    popFront' _
      = error "This InfQueue is not infinite!"

-- Look at the last element of the deque without changing the deque
peek :: QueueS a (Maybe a)
peek = do
  e <- pop
  if isNothing e
    then return e
    else do
      pushEnd (fromJust e)
      return e

-- Look at the first element of the deque without changing the deque
peekFront :: QueueS a (Maybe a)
peekFront = do
  e <- popFront
  if isNothing e
    then return e
    else do
      push (fromJust e)
      return e

-- Length of the deque
len :: Queue a -> Int
len (Queue inl _ ol _) 
  = inl + ol
len _ 
  = error "Cannot calculate length for InfQueue!"

-- Split the deque at the nth element (inclusive) from the front
splitAtQ :: Int -> Queue a -> (Queue a, Queue a)
splitAtQ n q = 
  splitAtQ' n emptyQueue q
  where
    splitAtQ' 0 tk dr
      = (tk, dr)
    splitAtQ' i tk (Queue _ [] _ [])
      = (tk, emptyQueue)
    splitAtQ' i tk dr
      = splitAtQ' (i - 1) tk' dr'
      where
        (Just e, dr') = runState popFront dr
        tk'      = execState (pushEnd e) tk

-- Split the deque at the nth element (inclusive) from the end
splitAtQEnd :: Int -> Queue a -> (Queue a, Queue a)
splitAtQEnd n q = 
  splitAtQEnd' n emptyQueue q
  where
    splitAtQEnd' 0 tk dr
      = (tk, dr)
    splitAtQEnd' i tk (Queue _ [] _ [])
      = (tk, emptyQueue)
    splitAtQEnd' i tk dr
      = splitAtQEnd' (i - 1) tk' dr'
      where
        (Just e, dr') = runState pop dr
        tk'      = execState (push e) tk

-- Take/drop the first nth element (inclusive)
takeQ, dropQ :: Int -> Queue a -> Queue a
takeQ
  = (fst .) . splitAtQ
dropQ
  = (snd .) . splitAtQ

-- Take/drop the last nth element (inclusive)
takeQEnd, dropQEnd :: Int -> Queue a -> Queue a
takeQEnd
  = (fst .) . splitAtQEnd
dropQEnd
  = (snd .) . splitAtQEnd
