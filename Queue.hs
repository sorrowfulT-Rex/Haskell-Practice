{-# LANGUAGE TupleSections #-}

module Queue where

-- Making a deque with amortised O(1) accesses

import           Control.Monad.Trans.State
import           Data.Maybe

-- Has an inbox (the first half of elements) and an outbox (the second half)
-- Ideally, both halves should have similar number of elements
-- Note that the Queue constructor is strict; for deque with infinite elements, us InfQueue
-- If you use InfQueue, make sure that it is indeed infinite (IMPORTANT!)
data Queue a = Queue Int ![a] Int ![a] | InfQueue [a] [a]

instance Show a => Show (Queue a) where
  show (Queue _ [] _ [])
    = "[]"
  show (Queue _ [a] _ [])
    = '[' : show a ++ "]"
  show (Queue _ [] _ [a])
    = '[' : show a ++ "]"
  -- Showing the first and the last elements
  show q
    = '[' : show (fromJust $ evalState peekFront q) ++ 
      "..." ++ 
      show (fromJust $ evalState peek q) ++ 
      "]"


-- Functor & Applicative Instances

instance Functor Queue where
  fmap f (Queue inl ins ol os)
    = Queue inl (map f ins) ol (map f os)
  fmap f (InfQueue ins os)
    = InfQueue (map f ins) (map f os)

instance Applicative Queue where
  -- In this implementation, pure f <*> a <*> b works like zipWith f a b
  pure x
    = InfQueue (repeat x) (repeat x)
  (<*>) (InfQueue ins os) (InfQueue ins' os')
    = InfQueue (zipWith ($) ins ins') (zipWith ($) os os')
  (<*>) (Queue _ [] _ []) _
    = emptyQueue
  (<*>) _ (Queue _ [] _ [])
    = emptyQueue
  (<*>) fs xs
    = execState (pushS (f x)) (fr <*> xr)
    where
      (Just f, fr) = runState popFrontS fs
      (Just x, xr) = runState popFrontS xs


-- Display functions

-- Convert the deque to list
toList :: Queue a -> [a]
toList (Queue _ ins _ os)
  = ins ++ reverse os
toList (InfQueue ins _)
  = ins

-- Print out the entire deque with its structure
showQueue :: Show a => Queue a -> String
showQueue (Queue _ ins _ os)
  = "inbox:  " ++ show ins ++ ";\noutbox: " ++ show os
showQueue (InfQueue ins _)
  = error "Cannot print an infinite queue; use take/takeEnd to see the first/last finite elements"


-- Initialisation

-- an empty deque
emptyQueue :: Queue a
emptyQueue
  = Queue 0 [] 0 []

-- Initialise a deque
makeQueue :: [a] -> Queue a
makeQueue as
  = Queue len fr len' (reverse re)
  where
    (len, r) = quotRem (length as) 2
    len'     = len + r
    (fr, re) = splitAt len as

-- Check if the deque is empty
empty :: Queue a -> Bool
empty (Queue _ [] _ [])
  = True
empty _
  = False


-- Access Functions

-- Helper function for pushing and popping
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

-- Length of the deque
len :: Queue a -> Int
len (Queue inl _ ol _)
  = inl + ol
len _ 
  = error "Cannot calculate length for InfQueue!"

-- Pushing an element to the front of the deque
push :: a -> Queue a -> Queue a
push e (Queue inl ins ol os) 
  = check (Queue (inl + 1) (e : ins) ol os)
push e (InfQueue ins os)
  = InfQueue (e : ins) os

-- Popping an element from the end of the deque
pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue _ [] _ [])
  = (Nothing, q)
pop (Queue _ [a] _ [])
  = (Just a, emptyQueue)
pop (Queue _ [] _ [a])
  = (Just a, emptyQueue)
pop (Queue inl ins ol (o : os))
  = (Just o, check (Queue inl ins (ol - 1) os))
pop (InfQueue ins (o : os))
  = (Just o, InfQueue ins os)
pop _
  = error "This InfQueue is not infinite!"

-- Pushing an element to the end of the deque
pushEnd :: a -> Queue a -> Queue a
pushEnd e (Queue inl ins ol os) 
  = check $ Queue inl ins (ol + 1) (e : os)
pushEnd e (InfQueue ins os)
  = InfQueue ins (e : os)

-- Popping an element from the front of the deque
popFront :: Queue a -> (Maybe a, Queue a)
popFront q@(Queue _ [] _ [])
  = (Nothing, q)
popFront (Queue _ [a] _ [])
  = (Just a, emptyQueue)
popFront (Queue _ [] _ [a])
  = (Just a, emptyQueue)
popFront (Queue inl (i : ins) ol os)
  = (Just i, check (Queue (inl - 1) ins ol os))
popFront (InfQueue (i : ins) os)
  = (Just i, InfQueue ins os)
popFront _
  = error "This InfQueue is not infinite!"


-- Operators

-- push
infixr 5 |>>
(|>>) :: a -> Queue a -> Queue a
(|>>) 
  = push

-- pushEnd
infixl 5 <<|
(<<|) :: Queue a -> a -> Queue a
(<<|) 
  = flip pushEnd

-- index from left
(!!<) :: Queue a -> Int -> a
(!!<) q n
  | n < 0     = error "Index out of bound!"
  | otherwise = index q n n
  where
    index q 0 n
      | Just e <- popped = e
      | otherwise        = error "Index out of bound!"
      where
        popped = evalState popFrontS q
    index q i n
      = index (execState popFrontS q) (i - 1) n

-- index from right
(!!>) :: Queue a -> Int -> a
(!!>) q n
  | n < 0     = error "Index out of bound!"
  | otherwise = index q n n
  where
    index q 0 n
      | Just e <- popped = e
      | otherwise        = error "Index out of bound!"
      where
        popped = evalState popS q
    index q i n
      = index (execState popS q) (i - 1) n


-- concat
infixr 5 +++
(+++) :: Queue a -> Queue a -> Queue a
(InfQueue ins os) +++ (Queue _ ins' _ os')
  = InfQueue ins (os' ++ reverse ins' ++ os)
(Queue _ ins _ os) +++ (InfQueue ins' os')
  = InfQueue (ins ++ reverse os ++ ins') os'
(Queue inl ins ol os) +++ (Queue inl' ins' ol' os')
  = check $ Queue (inl + ol) (ins ++ reverse os) (inl' + ol') (os' ++ reverse ins')
_ +++ _
  = error "Cannot concat two infinite queues!"


-- More functions

-- Split the deque at the nth element (inclusive) from the front
splitAtQ :: Int -> Queue a -> (Queue a, Queue a)
splitAtQ n q
  = splitAtQ' n emptyQueue q
  where
    splitAtQ' 0 tk dr
      = (tk, dr)
    splitAtQ' i tk (Queue _ [] _ [])
      = (tk, emptyQueue)
    splitAtQ' i tk dr
      = splitAtQ' (i - 1) tk' dr'
      where
        (Just e, dr') = runState popFrontS dr
        tk'           = execState (pushEndS e) tk

-- Split the deque at the nth element (inclusive) from the end
splitAtQEnd :: Int -> Queue a -> (Queue a, Queue a)
splitAtQEnd n q
  = splitAtQEnd' n emptyQueue q
  where
    splitAtQEnd' 0 tk dr
      = (tk, dr)
    splitAtQEnd' i tk (Queue _ [] _ [])
      = (tk, emptyQueue)
    splitAtQEnd' i tk dr
      = splitAtQEnd' (i - 1) tk' dr'
      where
        (Just e, dr') = runState popS dr
        tk'           = execState (pushS e) tk

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


-- State Functions

type QueueS a = State (Queue a)

-- Add element to the front of the deque
pushS :: a -> QueueS a ()
pushS
  = state . ((() ,) .) . push

-- Add element to the end of the deque
pushEndS :: a -> QueueS a ()
pushEndS
  = state . ((() ,) .) . pushEnd

-- Retrieve element from the end of the deque
popS :: QueueS a (Maybe a)
popS 
  = state pop

-- Retrieve element from the front of the deque
popFrontS :: QueueS a (Maybe a)
popFrontS
  = state popFront 

-- Look at the last element of the deque without changing the deque
peek :: QueueS a (Maybe a)
peek = do
  e <- popS
  if isNothing e
    then return e
    else do
      pushEndS (fromJust e)
      return e

-- Look at the first element of the deque without changing the deque
peekFront :: QueueS a (Maybe a)
peekFront = do
  e <- popFrontS
  if isNothing e
    then return e
    else do
      pushS (fromJust e)
      return e
