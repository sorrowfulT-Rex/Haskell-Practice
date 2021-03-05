import           Data.Array as A

-- Lazy; can cause heap exhaustion
mySum :: Num a => [a] -> a
mySum []
  = 0
mySum (x : xs)
  = x + mySum xs

-- ghci +RTS -M20m
-- mySum [1..10^6]
  -- Exception: heap overflow

-- A strict version
mySumStrict :: Num a => [a] -> a
mySumStrict xs
  = mss' xs 0
  where
    mss' [] s
      = s
    mss' (x : xs) s
      = let s' = s + x in s' `seq` mss' xs s' -- seq forces the evaluation of s'

-- Learning Process
-- Based on Haskell High Performance Programming by Samuli Thomasson

-- ghci +RTS -M20m
-- mySumStrict [1..10^6]
  -- 500000500000

-- In Haskell, a value can be evaluated to Normal Form (NF), 
-- which is fully evaluated; or a Weak Head Normal Form (WHNF), 
-- which evaluates up to the first data constructor.
-- seq only forces evaluation to WHNF.

-- Same to above but more concise
mySumStrict' :: Num a => [a] -> a
mySumStrict' xs
  = mss' xs 0
  where
    mss' [] s
      = s
    mss' (x : xs) s
      = mss' xs $! s + x -- f $! x = x `seq` f x

-- ghci +RTS -M20m
-- mySumStrict' [1..10^6]
  -- 500000500000

fibMem :: Int -> Integer
fibMem 
  = (map fib [0..] !!)
  where
    fib 0 = 1
    fib 1 = 1
    fib n 
      = fibMem (n - 2) + fibMem (n - 1)

-- This function has memoisation because fibMem is point-free; it is treated as
-- a Constant Applicative Form (CAF), which is either a constant or 
-- a combination of a bunch of constants, note that lambdas doesn't count.
-- A perk for point free!

fibNoMem :: Int -> Integer
fibNoMem x
  = map fib [0..] !! x
  where 
    fib 0 = 1
    fib 1 = 1
    fib n 
      = fibNoMem (n - 2) + fibNoMem (n - 1)

-- Now fibLambda is not a CAF, thus it's not going to have memoisation.

fibLambda :: Int -> Integer
fibLambda
  = \x -> map fib [0..] !! x
  where 
    fib 0 = 1
    fib 1 = 1
    fib n 
      = fibLambda (n - 2) + fibLambda (n - 1)

-- This works similarly as above.
-- If we want the compiler NOT to memorise something, we need to make that
-- function POINT-FULL.

-- However, the two functions above can be optimised by ghc during compilation.
-- If we don't want the compiler to turn it into CAF, we have to do some tricks.

fibTrulyNoMem :: a -> Int -> Integer
fibTrulyNoMem a x
  = a `seq` map fib [0..] !! x
  where 
    fib 0 = 1
    fib 1 = 1
    fib n 
      = fibTrulyNoMem a (n - 2) + fibTrulyNoMem a (n - 1)

-- Now even with 'ghc -O2 LayzeEvaluation.hs', the compiler won't be able to
-- turn the function above into CAF!
-- Since when do we need to fight the compiler as well...

fibArr :: Int -> Integer
fibArr 
  = (fmap fib' (array (0, 114514) $ zip [0..] [0..114514] :: Array Int Int) A.!)
  where
    fib' 0 = 1
    fib' 1 = 1
    fib' n = fibArr (n - 2) + fibArr (n - 1)

-- This function has super fast access to memoisation, but it has an upper bound
-- for the input.
