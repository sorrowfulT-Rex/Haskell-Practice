
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
