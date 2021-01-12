module Practice where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST

numero :: Int
numero = 17

toList :: Array Int a -> [a]
toList arr
  = toList' arr s e
  where
    (s, e) = bounds arr
    toList' arr i e
      | i > e     = []
      | otherwise = arr ! i : toList' arr (i + 1) e

zeroIndexArray :: Array Int Bool
zeroIndexArray 
  = array (0,9) ((3, True) : [(i, False) | i <- [0..9], i /= 3])

fifthElement :: Bool
fifthElement 
  = zeroIndexArray ! 4

positiveArray :: Array Int Int
positiveArray 
  = array (1, numero) $ zip [1..numero] [1..] 

seventhToThree :: Array Int Int
seventhToThree
  = positiveArray // [(7, 3)]

addEachByOne :: Array Int Int
addEachByOne = 
  accum (+) seventhToThree (zip [1..numero] (repeat 1))

listToArray :: [a] -> Array Int a
listToArray vals
  = runSTArray $ do
    let end = length vals - 1
    myArray <- newArray (0, end) (undefined :: a)
    writeIn myArray 0 end vals
    return myArray
    where
      writeIn _ _ _ []
        = return ()
      writeIn arr i e (v : vs) 
        | i > e     = return ()
        | otherwise = do
          writeArray arr i v
          writeIn arr (i + 1) e vs

bubbleSort :: (Ord a) => Array Int a -> Array Int a
bubbleSort myArray = runSTArray $ do
   stArray <- thaw myArray
   let (start, end) = bounds myArray
   forM_ [(start + 1) .. end] $ \i -> do
     forM_ [start .. (end - i)] $ \j -> do
       val <- readArray stArray j
       nextVal <- readArray stArray (j + 1)
       let outOfOrder = val > nextVal
       when outOfOrder $ do
         writeArray stArray j nextVal
         writeArray stArray (j + 1) val
   return stArray
