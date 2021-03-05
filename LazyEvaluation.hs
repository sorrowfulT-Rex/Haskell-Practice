{-# LANGUAGE BangPatterns #-}

-- Learning Process
-- Based on Haskell High Performance Programming by Samuli Thomasson

import           Control.Monad
import           Data.Array as A
import           Data.List (foldl')

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

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean :: [Double] -> Double
mean v = sum' v / fromIntegral (length v)

covariance :: [Double] -> [Double] -> Double
covariance xs ys 
  = sum' (zipWith (\x y -> (x - mean xs) * (y - mean ys)) xs ys) / len
  where
    len = fromIntegral $ length xs

-- main = do
--   let xs = [1, 1.1..500]
--   let ys = [2, 2.1..501]
--   print $ covariance xs ys

-- Uncomment the main function and compiler with 
-- "ghc -O0 -rtsopts LazyEvaluation.hs", we can pass options for Runtime System.
-- Use "./LazyEvaluation +RTS -s" to run the program, I get the following:
  -- 20758.399999999998
  --     802,779,632 bytes allocated in the heap
  --       1,218,520 bytes copied during GC
  --         427,896 bytes maximum residency (2 sample(s))
  --         84,104 bytes maximum slop
  --               0 MB total memory in use (0 MB lost due to fragmentation)

  --                                    Tot time (elapsed)  Avg pause  Max pause
  -- Gen  0       764 colls,     0 par    0.003s   0.003s     0.0000s    0.0005s
  -- Gen  1         2 colls,     0 par    0.000s   0.001s     0.0003s    0.0004s

  -- INIT    time    0.000s  (  0.006s elapsed)
  -- MUT     time    0.706s  (  0.711s elapsed)
  -- GC      time    0.003s  (  0.004s elapsed)
  -- EXIT    time    0.000s  (  0.005s elapsed)
  -- Total   time    0.709s  (  0.725s elapsed)

  -- %GC     time       0.0%  (0.0% elapsed)

  -- Alloc rate    1,137,790,736 bytes per MUT second

  -- Productivity  99.5% of total user, 98.1% of total elapsed
-- We can see from the first line that more than 765 MB has been allocated...
-- This is because "mean xs" and "mean ys" has been repeated calculated.

covariance' :: [Double] -> [Double] -> Double
covariance' xs ys 
  = sum' (zipWith (\x y -> (x - mxs) * (y - mys)) xs ys) / len
  where
    len = fromIntegral $ length xs
    mxs = mean xs
    mys = mean ys

-- main = do
--   let xs = [1, 1.1..500]
--   let ys = [2, 2.1..501]
--   print $ covariance' xs ys

-- Running with the same options, I get:
  -- 20758.399999999998
  --       2,703,024 bytes allocated in the heap
  --         586,224 bytes copied during GC
  --         44,408 bytes maximum residency (1 sample(s))
  --         29,320 bytes maximum slop
  --               0 MB total memory in use (0 MB lost due to fragmentation)

  --                                    Tot time (elapsed)  Avg pause  Max pause
  -- Gen  0         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0003s
  -- Gen  1         1 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  -- INIT    time    0.000s  (  0.005s elapsed)
  -- MUT     time    0.001s  (  0.001s elapsed)
  -- GC      time    0.001s  (  0.001s elapsed)
  -- EXIT    time    0.000s  (  0.008s elapsed)
  -- Total   time    0.002s  (  0.015s elapsed)

  -- %GC     time       0.0%  (0.0% elapsed)

  -- Alloc rate    2,439,552,346 bytes per MUT second

  -- Productivity  52.8% of total user, 8.4% of total elapsed
-- Now only 2.5 MB is allocated.

goGen, goGenShared :: Integer -> Integer
goGen u
  = sum [1..u] + product [1..u]
goGenShared u
  = let xs = [1..u] in sum xs + product xs

-- main = do
--   let !a = goGen 114514
--   return ()

-- main = do
--   let !a = goGenShared 114514
--   return ()

-- In the first example, we have:
  -- 15,063,994,536 bytes allocated in the heap
  --  3,262,380,872 bytes copied during GC
  --      7,374,616 bytes maximum residency (3614 sample(s))
  --        102,776 bytes maximum slop
  --              7 MB total memory in use (0 MB lost due to fragmentation)

  --                                    Tot time (elapsed)  Avg pause  Max pause
  -- Gen  0      9864 colls,     0 par    0.090s   0.097s     0.0000s    0.0016s
  -- Gen  1      3614 colls,     0 par    2.770s   2.811s     0.0008s    0.0072s

  -- INIT    time    0.000s  (  0.006s elapsed)
  -- MUT     time    1.381s  (  1.407s elapsed)
  -- GC      time    2.860s  (  2.908s elapsed)
  -- EXIT    time    0.000s  (  0.007s elapsed)
  -- Total   time    4.241s  (  4.328s elapsed)

  -- %GC     time       0.0%  (0.0% elapsed)

  -- Alloc rate    10,911,028,106 bytes per MUT second

  -- Productivity  32.6% of total user, 32.5% of total elapsed

-- In the second example, we have:
  -- 15,055,749,480 bytes allocated in the heap
  --  8,858,062,128 bytes copied during GC
  --     10,153,640 bytes maximum residency (1734 sample(s))
  --        793,496 bytes maximum slop
  --              9 MB total memory in use (0 MB lost due to fragmentation)

  --                                    Tot time (elapsed)  Avg pause  Max pause
  -- Gen  0     11736 colls,     0 par    0.106s   0.114s     0.0000s    0.0014s
  -- Gen  1      1734 colls,     0 par    4.820s   4.869s     0.0028s    0.0101s

  -- INIT    time    0.000s  (  0.005s elapsed)
  -- MUT     time    1.401s  (  1.427s elapsed)
  -- GC      time    4.926s  (  4.982s elapsed)
  -- EXIT    time    0.000s  (  0.011s elapsed)
  -- Total   time    6.327s  (  6.424s elapsed)

  -- %GC     time       0.0%  (0.0% elapsed)

  -- Alloc rate    10,749,062,390 bytes per MUT second

  -- Productivity  22.1% of total user, 22.2% of total elapsed

-- In comparison, we can see that the shared version is almost 50% slower, and
-- spends 70% more time on Garbage Collection. This is because in the shared
-- version we have to keep a very large list in the heap, while in the first
-- example we comsumes elements as we go.

goGenOnePass :: Integer -> Integer
goGenOnePass u 
  = s + p
  where
    (s, p) = foldl' f (0, 1) [1..u]
    f (s, p) i
      = let s' = s + i; p' = p * i in s' `seq` p `seq` (s', p')

-- main = do
--   let !a = goGenOnePass 114514
--   return ()

-- This time:
  -- 15,045,366,072 bytes allocated in the heap
  --     1,356,680 bytes copied during GC
  --         36,088 bytes maximum residency (2 sample(s))
  --         29,448 bytes maximum slop
  --             0 MB total memory in use (0 MB lost due to fragmentation)

  --                                    Tot time (elapsed)  Avg pause  Max pause
  -- Gen  0     13446 colls,     0 par    0.028s   0.034s     0.0000s    0.0001s
  -- Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  -- INIT    time    0.000s  (  0.005s elapsed)
  -- MUT     time    1.249s  (  1.272s elapsed)
  -- GC      time    0.028s  (  0.035s elapsed)
  -- EXIT    time    0.000s  (  0.005s elapsed)
  -- Total   time    1.277s  (  1.317s elapsed)

  -- %GC     time       0.0%  (0.0% elapsed)

  -- Alloc rate    12,048,659,601 bytes per MUT second

  -- Productivity  97.8% of total user, 96.6% of total elapsed
-- It's much more efficient because we make only one pass through all elements
-- while consuming them at the same time.
