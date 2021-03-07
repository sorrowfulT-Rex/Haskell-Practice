{-# LANGUAGE BangPatterns #-}

-- Learning Process
-- Based on Haskell High Performance Programming by Samuli Thomasson

len :: [a] -> Int
len (_ : xs) 
  = 1 + len xs
len _
  = 0

lenStrict :: [a] -> Int
lenStrict xs
  = len' 0 xs
  where
    len' !s (_ : xs) 
      = len' (s + 1) xs
    len' s _
      = s

-- Add '!' before a field to ensure strictness.

data D = D !Int

-- The above is allowed without extensions.

data E = E {-# UNPACK #-} !Int

-- By using '{-# UNPACK #-}', the following field is no longer a pointer, in
-- other word, it is more efficient. Unpacked field must be wrapped primitives,
-- and they should be strict (with '!').

data PairP = PairP Int Int 
  deriving Show
data PairS = PairS !Int !Int 
  deriving Show
data PairU = PairU {-# UNPACK #-} !Int {-# UNPACK #-} !Int 
  deriving Show

iter :: Int -> (a -> a) -> a -> a
iter n f x
  = iter' n x
  where
    iter' 0 x = x
    iter' i x
      = iter' (i - 1) $! f x

-- Consider compiling the following six main functions without optimisation.

-- main 
--   = print $ iter 10000000 f (PairP 0 0)
--   where
--     f (PairP x y)
--       = PairP (x + 2) (y + 1)

-- 2.882 s; 652 MB memory.

-- main 
--   = print $ iter 10000000 f (0, 0)
--   where
--     f (x, y)
--       = (x + 2, y + 1)

-- 3.701 s; 674 MB memory.

-- main 
--   = print $ iter 10000000 f (0, 0)
--   where
--     f :: (Int, Int) -> (Int, Int)
--     f (x, y)
--       = (x + 2, y + 1)

-- 2.876 s; 652 MB memory.

-- main 
--   = print $ iter 10000000 f (PairP 0 0)
--   where
--     f (PairP x y)
--       = PairP (x `seq` (x + 2)) (y `seq` (y + 1))

-- 2.610 s; 568 MB memory.

-- main 
--   = print $ iter 10000000 f (PairS 0 0)
--   where
--     f (PairS x y)
--       = PairS (x + 2) (y + 1)

-- 0.575 s; 0 MB memory.

-- main 
--   = print $ iter 10000000 f (PairU 0 0)
--   where
--     f (PairU x y)
--       = PairU (x + 2) (y + 1)

-- 0.577 s; 0 MB memory.

-- The first main uses the user-defined lazy tuple. Although we have 
-- deliberately iter strict, there is still a long stack of expressions building
-- up in the memory. The reason is that ($!) only evaluates the pair up to its
-- WHNF, or Pair _ _, while the expression inside remains untouched.

-- The second main class uses the default tuple. It is essentially the same as
-- above, but is actually slower due to polymorphism.

-- The third main erases polymorphism and is almost identical with the first
-- main in terms of performance.

-- The fourth main is similar to the first one as well; the strictness within
-- f does not help preventing the formation of a large chain of expressions due
-- to WHNF evaluation.

-- The fifth main is "fully strict", whith the bang annotation, whenever PairS
-- is evaluated to its WHNF, everything inside is evaluated as well. Thus this
-- one if much faster.

-- In the last example, unboxing the Ints does not have an impact to efficiency.
-- However, unboxing would make a difference in situations such as mutable array
-- manipulation.

data Tuple = Tuple {-# UNPACK #-} !Int {-# UNPACK #-} !Int 
  deriving Show
data Tuple2 = Tuple2 {-# UNPACK #-} !(Int, Int)
  deriving Show

-- main
--   = print $ iter 10000000 f (Tuple 0 0)
--   where
--     f (Tuple x y)
--       = (Tuple (x + 1) (y + 2))

-- main
--   = print $ iter 10000000 f (Tuple2 (0, 0))
--   where
--     f (Tuple2 (x, y))
--       = (Tuple2 (x + 1, y + 2))

-- The first main is much faster than the second one, again because Tuple is
-- strict on each Ints while Tuple only forces evaluation on the tuple, where
-- the actual Ints inside the tuple are still lazy as sloth.
