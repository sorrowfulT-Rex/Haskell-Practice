{-# LANGUAGE BangPatterns #-}

module BTree where

import           Data.List

-- Ordered Binary Search Tree

data BSTree a = BSEmpty | BSLeaf a | BSNode Int (BSTree a) a Int (BSTree a)
  deriving (Show, Eq)

instance Functor BSTree where
  fmap _ BSEmpty
    = BSEmpty
  fmap f (BSLeaf a)
    = BSLeaf (f a)
  fmap f (BSNode ld l a rd r)
    = BSNode ld (f <$> l) (f a) rd (f <$> r)

instance Foldable BSTree where
  foldMap _ BSEmpty
    = mempty
  foldMap f (BSLeaf a)
    = f a
  foldMap f (BSNode _ l a _ r)
    = foldMap f l `mappend` f a `mappend` foldMap f r

depth :: BSTree a -> Int
depth (BSNode ld _ _ rd _)
  = max ld rd
depth _
  = 0

toList :: BSTree a -> [a]
toList
  = foldMap (: [])


-- Initialisation

-- Empty Tree
emptyBST :: BSTree a
emptyBST = BSEmpty

-- Initialise from list
makeBST :: Ord a => [a] -> BSTree a
makeBST
  = foldl' (flip insertBST) emptyBST


-- Add/Delete

-- Balancing a tree after an insertion/deletion
balance :: Ord a => BSTree a -> BSTree a
balance (BSNode ld (BSNode ld' l' a' rd' r') a rd r)
  | ld > rd + 1 = if ld' > rd'
    then BSNode ld' l' a' (rd + 1) (BSNode rd' r' a rd r)
    else BSNode ld' (BSNode ld' l' a' ld'' l'') a'' (rd + 1) (BSNode rd'' r'' a rd r)
      where
        BSNode ld'' l'' a'' rd'' r'' = r'
balance (BSNode ld l a rd (BSNode ld' l' a' rd' r'))
  | rd > ld + 1 = if ld' < rd'
    then BSNode (ld + 1) (BSNode ld l a ld' l') a' rd' r'
    else BSNode (ld + 1) (BSNode ld l a ld'' l'') a'' rd' (BSNode rd'' r'' a' rd' r')
      where
        BSNode ld'' l'' a'' rd'' r'' = l'
balance tree
  = tree

-- Add element to the BST
insertBST :: Ord a => a -> BSTree a -> BSTree a
insertBST e BSEmpty
  = BSLeaf e
insertBST e az@(BSLeaf a)
  | e < a     = BSNode 0 BSEmpty e 1 az
  | otherwise = BSNode 1 az e 0 BSEmpty
insertBST e (BSNode ld l a rd r)
  | e < a     = balance $ BSNode (depth lInsert + 1) lInsert a rd r
  | otherwise = balance $ BSNode ld l a (depth rInsert + 1) rInsert
  where
    lInsert = insertBST e l
    rInsert = insertBST e r

-- Add element and replace duplicates
updateBST :: Ord a => a -> BSTree a -> BSTree a
updateBST e BSEmpty
  = BSLeaf e
updateBST e az@(BSLeaf a)
  | e < a     = BSNode 0 BSEmpty e 1 az
  | e > a     = BSNode 1 az e 0 BSEmpty
  | otherwise = BSLeaf e
updateBST e (BSNode ld l a rd r)
  | e < a     = balance $ BSNode (depth lUpdate + 1) lUpdate a rd r
  | e > a     = balance $ BSNode ld l a (depth rUpdate + 1) rUpdate
  | otherwise = BSNode ld l a rd r
  where
    lUpdate = updateBST e l
    rUpdate = updateBST e r

-- Check if an element is in the BST
elemBST :: Ord a => a -> BSTree a -> Bool
elemBST _ BSEmpty
  = False
elemBST e (BSLeaf a)
  = e == a
elemBST e (BSNode _ l x _ r)
  | e < x     = elemBST e l
  | e > x     = elemBST e r
  | otherwise = True
