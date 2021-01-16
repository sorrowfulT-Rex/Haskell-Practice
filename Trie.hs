module HashSet where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.List hiding (lookup, insert, delete)
import           Data.Maybe
import           Data.Sequence (
  Seq (..), 
  empty, 
  singleton, 
  fromList, 
  lookup, 
  insertAt, 
  update,
  deleteAt,
  elemIndexL
  )
import           Prelude hiding (lookup)

-- import           BTree


-- Types

type BitVector = Int
type Hash = Int
type HashFun = Int -> Hash
type HashSet = Trie

data Trie a = Leaf (Seq a) | Node BitVector (Seq (Trie a))
  deriving (Eq, Show)

instance Foldable Trie where
  foldMap f (Leaf ls)
    = foldMap f ls
  foldMap f (Node _ subT)
    = foldl' ((. (foldMap f)) . (flip mappend)) mempty subT

figure :: Trie Int
figure
  = Node 16960 (fromList [Leaf (singleton 1830),
    (Node 8208 (fromList [Leaf (singleton 73),
    (Leaf (fromList [729,2521]))])),
    Leaf (singleton 206)])


-- Initialisation

emptyTrie :: Hashable a => Trie a
emptyTrie = Leaf empty

buildTrie :: Hashable a => [a] -> Trie a
buildTrie
  = foldl' (flip insert) emptyTrie


-- Methods

isEmpty :: Trie a -> Bool
isEmpty (Leaf Empty)
  = True
isEmpty _
  = False

insert :: Hashable a => a -> Trie a -> Trie a
insert
  = execState . insertT

remove :: Hashable a => a -> Trie a -> Trie a
remove
  = execState . removeT

member :: Hashable a => a -> Trie a -> Bool
member e trie
  = member' h trie 0
  where
    h = hash e
    member' _ (Leaf ls) _
      = e `elem` ls
    member' h (Node bv subtries) depth
      | hasEntry  = member' h (subtries ! (countOnesFrom hSeg bv)) (depth + 1)
      | otherwise = False
      where
        hSeg     = getIndex h depth 4
        hasEntry = testBit bv hSeg

pop (Leaf Empty)
  = (Nothing, emptyTrie)
pop trie
  = (Just e, remove e trie)
  where
    e = findElem trie
    findElem (Leaf (l :<| _))
      = l
    findElem (Node _ (t :<| _))
      = findElem t


-- State Methods

type TrieT a = State (Trie a)
type HashMapT a = TrieT a

insertT :: Hashable a => a -> TrieT a Bool
insertT a
  = state $ flip insert' 0
  where
    insert' (Leaf Empty) _
      = (True, Leaf $ singleton a)
    insert' trie@(Leaf lss@(l :<| ls)) d
      | l == a        = (False, Leaf (a :<| ls))
      | hSeg == hSeg' = (True, Leaf (a :<| lss))
      | hSeg < hSeg'  = (True, Node bv (fromList [Leaf (singleton a), trie]))
      | otherwise     = (True, Node bv (fromList [trie, Leaf (singleton a)]))
      where
        h     = hash a
        h'    = hash l
        hSeg  = getIndex h d 4
        hSeg' = getIndex h' d 4
        bv    = bit hSeg + bit hSeg'
    insert' (Node bv subT) d
      | hasEntry  = (v, Node bv subT')
      | otherwise = (True, Node bv' subT'')
      where
        hSeg       = getIndex (hash a) d 4
        hasEntry   = testBit bv hSeg
        entry      = countOnesFrom hSeg bv
        subN       = subT ! entry
        (v, subN') = insert' subN (d + 1)
        subT'      = update entry subN' subT
        bv'        = bv + bit hSeg
        subT''     = insertAt entry (Leaf $ singleton a) subT

removeT :: Hashable a => a -> TrieT a Bool
removeT a
  = state $ flip remove' 0
  where
    remove' (Leaf Empty) _
      = (False, emptyTrie)
    remove' trie@(Leaf ls) _
      | member a trie = (True, Leaf $ delete a ls)
      | otherwise     = (False, trie)
    remove' trie@(Node bv subT) d
      | hasEntry  = (v, trie')
      | otherwise = (False, trie)
      where
        hSeg       = getIndex (hash a) d 4
        hasEntry   = testBit bv hSeg
        entry      = countOnesFrom hSeg bv
        subN       = subT ! entry
        (v, subN') = remove' subN (d + 1)
        subT'      = update entry subN' subT
        bv'        = bv - bit hSeg
        subT''     = deleteAt entry subT
        trie'
          | not $ isEmpty subN' = Node bv subT'
          | bv' > 0             = Node bv' subT''
          | otherwise           = emptyTrie

popT :: Hashable a => TrieT a (Maybe a)
popT
  = state pop


-- Utilities

infixl 7 !
(!) :: Seq a -> Int -> a
(!)
  = (fromJust .) . (flip lookup)

delete :: Eq a => a -> Seq a -> Seq a
delete 
  = join . ((deleteAt . fromJust) .) . elemIndexL

bitTable :: Seq Int
bitTable 
  = fromList [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = countOnes n'
  where
    n' = (bit i - 1) .&. n
    countOnes 0
      = 0
    countOnes n
      = countOnes q + bitTable ! r
      where
        (q, r) = quotRem n 16

getIndex :: Int -> Int -> Int -> Int
getIndex n 0 b
  = (bit b - 1) .&. n
getIndex n i b
  = getIndex n' (i - 1) b
  where
    n' = shiftR n b


-- Hashables

class Eq a => Hashable a where
  hash :: a -> Int

instance Hashable Bool where
  hash True = 1
  hash _    = 0

instance Hashable Int where
  hash = id

instance Hashable Integer where
  hash
    = fromIntegral . (flip mod $ fromIntegral (maxBound :: Int))

instance Hashable Char where
  hash = ord

instance Hashable a => Hashable [a] where
  hash []
    = 0
  hash (x : _)
    = hash x

instance (Hashable a, Eq b) => Hashable (a, b) where
  hash (a, b) = hash a
