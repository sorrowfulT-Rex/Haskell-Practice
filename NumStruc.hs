module NumStruc where

import           Control.Monad.Trans.State
import           Data.Maybe
import           Prelude hiding (fst, snd)
import           System.IO (IOMode(..), openFile)
import           System.IO.Unsafe

-- Dependency: $ cabal install --lib strict-io
import           System.IO.Strict hiding (read, appendFile, openFile, print)

import           Queue

-- We say a set S of natural integers is an accumulative structure if for any n <- [0..],
-- we can determine if n is an element of S when we all the elements of S smaller than n.
-- We call this determining rule the accumulation filter.

-- For example, the set of all prime numbers is accumulative, since we can tell if a number is prime
-- when we have all the primes below that number.

-- Ulam numbers are also accumulative. The list of all ulam numbers is defined to start with
-- 1 and 2, and any other ulam number can be represented as the sum of two distinct (smaller) ulam numbers.

-- In the case of ulam numbers, we start from [1, 2], which is not determined by the rule,
-- we call it the base case.
-- For primes the base case is simply []

-- This program calculates the infinite list of any accumulative structure, writes the list into a file,
-- and can read previously calculated results to avoid repetitive computation.

-- Produces an infinite accumulative structure from an accumulation filter and a base
filterAcc :: (a -> QueueS a Bool) -> [a] -> Queue a -> [a]
filterAcc _ [] _
  = []
filterAcc f (a : as) q 
  | passed    = a : filterAcc f as q'
  | otherwise = filterAcc f as q
  where
    passed = evalState (f a) q
    q'     = execState (pushS a) q

-- Read previous calculations
-- The second argument is the (fixed) list of base case, for example for ulam numbers it is [1, 2]
numRead :: FilePath -> [Integer] -> IO [Integer]
numRead path base = do
  rd <- numRead' path
  return $ base ++ rd

numRead' :: FilePath -> IO [Integer]
numRead' path = do
  h   <- openFile path ReadWriteMode
  raw <- run $ hGetContents h
  return $ map read (words raw)

-- Generate an infinite list of accumulative structure starting from (n + 1), given all the elements <= n
-- The first argument is the (fixed) list of base case.
-- the second argument is the smallest (first) number in the structure after the base case,
-- (e.g. for prime it is 2 and for ulam it is 3),
-- and the third argument is the accumulation filter
-- Using self-defined deque structure
numGen :: [Integer] -> Integer -> (Integer -> QueueS Integer Bool) -> State [Integer] [Integer]
numGen base d f = do
  lst <- get
  let queue = makeQ (reverse (base ++ lst))
  let max   = evalState popFrontS queue
  let start = maybe d succ max
  put $ filterAcc f [start..] queue
  return (base ++ lst)

-- Read from files of previous computation results, do new calculations, and simultenously store new results
numListIO :: FilePath -> [Integer] -> Integer -> (Integer -> QueueS Integer Bool) -> IO [Integer]
numListIO path base d f = do
  lst <- numRead' path
  let lst' = execState (numGen base d f) lst
  rst <- numListW lst' False
  return $ base ++ lst ++ rst
  where
    numListW nz@(n : ns) flag 
      | flag      = do
        appendFile path (' ' : show n)
        rst <- unsafeInterleaveIO $ numListW ns True
        return $ n : rst
      | otherwise = unsafeInterleaveIO $ numListW nz True
