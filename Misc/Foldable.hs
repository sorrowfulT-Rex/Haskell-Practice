{-# LANGUAGE NoImplicitPrelude #-}

import Data.Semigroup hiding (Endo, appEndo)
import Data.Monoid hiding (Endo, appEndo)
import Prelude 
  (Bool, Eq, Int, Ord, Num, flip, fmap, id, (.), ($), (+), (*), ($!))

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
  -- For example, suppose xs = [a1, a2, a3], then foldl' f c xs is
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
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldr' f c xs = foldl f' id xs c
    where
    f' m x z = m $! f x z

  -- foldr1 :: (a -> a -> a) -> t a -> a
  -- foldl1 :: (a -> a -> a) -> t a -> a
  -- toList :: t a -> [a]
  -- null :: t a -> Bool
  -- length :: t a -> Int

instance Foldable [] where
  foldMap f xs = mconcat $ fmap f xs
