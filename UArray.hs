module Practice where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed

numero :: Int
numero = 17

toList :: UArray Int Int -> [Int]
toList arr
  = toList' arr s e
  where
    (s, e) = bounds arr
    toList' arr i e
      | i > e     = []
      | otherwise = arr ! i : toList' arr (i + 1) e

zeroIndexArray :: UArray Int Bool
zeroIndexArray 
  = array (0,9) [(3,True)]

fifthElement :: Bool
fifthElement 
  = zeroIndexArray ! 4

positiveArray :: UArray Int Int
positiveArray 
  = array (1, numero) $ zip [1..numero] [1..] 

seventhToThree :: UArray Int Int
seventhToThree
  = positiveArray // [(7, 3)]

addEachByOne :: UArray Int Int
addEachByOne = 
  accum (+) seventhToThree (zip [1..numero] (repeat 1))

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    myArray <- newArray (0, end) 0
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

listToUArray :: [Int] -> UArray Int Int
listToUArray val
  = runSTUArray $ listToSTUArray val

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
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
