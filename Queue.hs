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
    = '[' : show (q !!< 0) ++ 
      "..." ++ 
      show (q !!> 0) ++ 
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
    = emptyQ
  (<*>) _ (Queue _ [] _ [])
    = emptyQ
  (<*>) fs xs
    = execState (pushS (f x)) (fr <*> xr)
    where
      (Just f, fr) = runState popFrontS fs
      (Just x, xr) = runState popFrontS xs


-- Display functions

-- Convert the deque to list from left to right
toListL :: Queue a -> [a]
toListL (Queue _ ins _ os)
  = ins ++ reverse os
toListL (InfQueue ins _)
  = ins

-- Convert the deque to list from right to left
toListR :: Queue a -> [a]
toListR (Queue _ ins _ os)
  = os ++ reverse ins
toListR (InfQueue _ os)
  = os

-- Print out the entire deque with its structure
showQ :: Show a => Queue a -> String
showQ (Queue inl ins ol os)
  = "Inbox (length " ++ show inl ++ "):  " ++ show ins ++ ";\nOutbox (length " ++ show ol ++ "): " ++ show os
showQ _
  = error "Cannot print an infinite queue; use take/takeEnd to see the first/last finite elements"


-- Initialisation

-- an empty deque
emptyQ :: Queue a
emptyQ
  = Queue 0 [] 0 []

-- Initialise a deque
makeQ :: [a] -> Queue a
makeQ as
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
  = (Just a, emptyQ)
pop (Queue _ [] _ [a])
  = (Just a, emptyQ)
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
  = (Just a, emptyQ)
popFront (Queue _ [] _ [a])
  = (Just a, emptyQ)
popFront (Queue inl (i : ins) ol os)
  = (Just i, check (Queue (inl - 1) ins ol os))
popFront (InfQueue (i : ins) os)
  = (Just i, InfQueue ins os)
popFront _
  = error "This InfQueue is not infinite!"

-- Get element at index from left
indexL :: Queue a -> Int -> Maybe a
indexL q n
  | n < 0     = Nothing
  | n == 0    = e
  | otherwise = indexL q' (n - 1)
  where
    (e, q') = popFront q

-- Get element at index from right
indexR :: Queue a -> Int -> Maybe a
indexR q n
  | n < 0     = Nothing
  | n == 0    = e
  | otherwise = indexR q' (n - 1)
  where
    (e, q') = pop q

-- Contatenate two deques
concatQ :: Queue a -> Queue a -> Queue a
concatQ (InfQueue ins os) (Queue _ ins' _ os')
  = InfQueue ins (os' ++ reverse ins' ++ os)
concatQ (Queue _ ins _ os) (InfQueue ins' os')
  = InfQueue (ins ++ reverse os ++ ins') os'
concatQ (Queue inl ins ol os) (Queue inl' ins' ol' os')
  = check $ Queue (inl + ol) (ins ++ reverse os) (inl' + ol') (os' ++ reverse ins')
-- When concatenating two infinite deques, the outbox of the left deque and the inbox of the right deque
-- are discarded since they are unaccessible.
concatQ (InfQueue ins _) (InfQueue _ os)
  = InfQueue ins os


-- Operators

-- push
infixr 4 >:>
(>:>) :: a -> Queue a -> Queue a
(>:>) 
  = push

-- pushEnd
infixl 5 <:<
(<:<) :: Queue a -> a -> Queue a
(<:<) 
  = flip pushEnd

-- indexL
(!!<) :: Queue a -> Int -> a
(!!<)
  = (maybe (error "Index out of bound!") id .) . indexL

-- indexR
(!!>) :: Queue a -> Int -> a
(!!>) 
  = (maybe (error "Index out of bound!") id .) . indexR

-- concat (left associative)
infixl 4 +<+
(+<+) :: Queue a -> Queue a -> Queue a
(+<+)
  = concatQ

-- concat (right associative)
infixr 5 +>+
(+>+) :: Queue a -> Queue a -> Queue a
(+>+)
  = concatQ


-- More functions

-- Reverse the deque
reverseQ :: Queue a -> Queue a
reverseQ (Queue inl ins ol os)
  = Queue ol os inl ins
reverseQ (InfQueue ins os)
  = InfQueue os ins

-- Split the deque at the nth element (inclusive) from the front
splitAtQL :: Int -> Queue a -> (Queue a, Queue a)
splitAtQL n q
  = splitAtQL' n emptyQ q
  where
    splitAtQL' 0 tk dr
      = (tk, dr)
    splitAtQL' i tk (Queue _ [] _ [])
      = (tk, emptyQ)
    splitAtQL' i tk dr
      = splitAtQL' (i - 1) tk' dr'
      where
        (Just e, dr') = runState popFrontS dr
        tk'           = execState (pushEndS e) tk

-- Split the deque at the nth element (inclusive) from the end
splitAtQR :: Int -> Queue a -> (Queue a, Queue a)
splitAtQR n q
  = splitAtQR' n emptyQ q
  where
    splitAtQR' 0 tk dr
      = (tk, dr)
    splitAtQR' i tk (Queue _ [] _ [])
      = (tk, emptyQ)
    splitAtQR' i tk dr
      = splitAtQR' (i - 1) tk' dr'
      where
        (Just e, dr') = runState popS dr
        tk'           = execState (pushS e) tk

-- Take/drop the first nth element (inclusive)
takeQL, dropQL :: Int -> Queue a -> Queue a
takeQL
  = (fst .) . splitAtQL
dropQL
  = (snd .) . splitAtQL

-- Take/drop the last nth element (inclusive)
takeQR, dropQR :: Int -> Queue a -> Queue a
takeQR
  = (fst .) . splitAtQR
dropQR
  = (snd .) . splitAtQR


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
