{-# LANGUAGE NoImplicitPrelude #-}

import Control.Applicative
import Data.Bool
import Data.Function
import Data.Semigroup hiding (Endo, appEndo)
import Data.Maybe
import Data.Monoid hiding (Endo, appEndo)
import Prelude 
  ( Bool, Eq, Int, Ord, Num, Show, flip, fmap, even, id, (.), ($), (+), (*)
  , ($!), (==)
  )

newtype Endo a = Endo { appEndo :: a -> a }

newtype FlipEndo a = FlipEndo { appFlipEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo a <> Endo b = Endo $ a . b

instance Monoid (Endo a) where
  mempty = Endo id

instance Semigroup (FlipEndo a) where
  FlipEndo a <> FlipEndo b = FlipEndo $ b . a

instance Monoid (FlipEndo a) where
  mempty = FlipEndo id

-- instance Mon

-- Implementing Foldable
class Foldable t where
  -- Turn every element in the Foldable to a monoid, then combine them.
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f = foldr (mappend . f) mempty

  -- We can view a -> b -> b as a -> (b -> b), in other words, every element
  -- produces an endomorphism on b.
  -- If we compose those endomorphisms and apply it to the initial value, we've
  -- got foldr.
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f c xs = appEndo (foldMap (Endo . f) xs) c

  fold :: Monoid m => t m -> m
  fold = foldMap id

  -- For foldl, we first flip f into a -> (b -> b), then we can compose them in
  -- the opposite direction.
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f c xs = appFlipEndo (foldMap (FlipEndo . flip f) xs) c

  -- There is another way to implement foldl (from foldr).
  -- Here the f' function takes an element of the Foldable and an endomorphism
  -- on b, returning another b -> b. 
  -- For example, suppose xs = [a1, a2, a3], then foldr' f c xs is
  -- f' a1 (f' a2 (f' a3 id)) c. Now f' a3 id is \e -> f' a3 id e, or
  -- \e -> id $! f e a3 = \e -> f e a3.
  -- f' a2 (f' a3 id) is \e' -> f' a2 (\e -> f e a3) e' =
  -- \e' -> (\e -> f e a3) $! f e' a2 =
  -- \e' -> f (f e' a2) a3.
  -- Keep going, we have f' a1 (f' a2 (f' a3 id)) is \e -> f (f (f e a1) a2) a3.
  -- Substitute e with c, we have f (f (f c a1) a2) a3, which is the semantic of
  -- foldl.
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldl' f c xs = foldr f' id xs c
    where
      f' x m z = m $! f z x

  -- Similarly.
  -- Suppose xs = [a1, a2, a3], then foldl' f c xs is
  -- f' (f' (f' id a1) a2) a3 c. Now f' id a1 is \e -> f' id a1 e, which is
  -- \e -> f a1 e or f a1. f' (f' id a1) a2 is f' (f a1) a2, or
  -- \e -> f a1 (f a2 e).
  -- We can expand it out and the result is the same.
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldr' f c xs = foldl f' id xs c
    where
    f' m x z = m $! f x z

  foldr1 :: (a -> a -> a) -> t a -> a
  foldr1 f xs = fromJust $ foldr f' Nothing xs
    where
      f' x Nothing   = Just x
      f' x (Just x') = Just $ f x x'

  foldl1 :: (a -> a -> a) -> t a -> a
  foldl1 f xs = fromJust $ foldl f' Nothing xs
    where
      f' Nothing x   = Just x
      f' (Just x) x' = Just $ f x x'

  toList :: t a -> [a]
  toList = foldr (:) []

  null :: t a -> Bool
  null = foldr (const $ const False) True

  length :: t a -> Int
  length = foldl' (\acc _ -> acc + 1) 0

instance Foldable [] where
  foldMap f xs = mconcat $ fmap f xs


-- Fold exercises

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

concat :: Foldable t => t [a] -> [a]
concat = fold

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap = foldMap

all :: Foldable t => (a -> Bool) -> t a -> Bool
all f xs = getAll (foldMap (All . f) xs)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x xs = getAny (foldMap (Any . ( == x)) xs)

len :: Foldable t => t a -> Int
len = getSum . foldMap (const $ Sum 1)


-- Foldable laws:

-- foldMap is the same as applying f to each element then fold.
  -- 1. foldMap f = fold . fmap f

-- Applying a function (g) on a fold on another function f is the same as
-- applying the composition of the two functions (g . f) then fold.
  -- 2. foldMap (g . f) = g . foldMap f

-- Derived laws:

-- Fold on a function (g) after mapping f is the same as applying g to the
-- monolithic value obtained by foldMap f.
  -- 3. foldMap g . fmap f = g . foldMap f
-- Proof:
  --   foldMap g . fmap f
  -- = fold . fmap g . fmap f         by 1)
  -- = fold . fmap (g . f)            by Functor law 2)
  -- = foldMap (g . f)                by 1)
  -- = g . foldMap f                  by 2)
