module SHA1 where

-- Simulating a SHA-1 Hashing

import           Data.Binary (Word8, Word32)
import           Data.Bits
  (FiniteBits, complement, rotateL, shiftL, xor, (.&.), (.|.))
import           Data.Char (ord)
import           Data.List (foldl')

encrypt :: String -> Integer
encrypt = sha1 . preLoop . stringToASCII

h0, h1, h2, h3, h4 :: Word32
h0 = 0x67452301
h1 = 0xEFCDAB89
h2 = 0x98BADCFE
h3 = 0x10325476
h4 = 0xC3D2E1F0

c0, c1, c2, c3 :: Word32
c0 = 0x5A827999
c1 = 0x6ED9EBA1
c2 = 0x8F1BBCDC
c3 = 0xCA62C1D6

stringToASCII :: String -> [Word8]
stringToASCII = map (fromIntegral . ord)

preLoop :: (FiniteBits a, Num a) => [a] -> [[a]]
preLoop cd
  = chunk 64 $
    cd ++
    shiftL 1 7 : replicate ((55 - length cd) `mod` 64) 0 ++
    let sp = split256 (8 * length cd) in replicate (8 - length sp) 0 ++ sp

chunk :: (Eq t, Num t) => t -> [a] -> [[a]]
chunk s xs
  = chunk' 0 xs
  where
    chunk' _ []
      = [[]]
    chunk' i xz@(x : xs)
      | i == s    = [] : chunk' 0 xz
      | otherwise = let (c : cs) = chunk' (i + 1) xs in (x : c) : cs

split256 :: (FiniteBits a, Num a) => Int -> [a]
split256 i
  | i < 256   = [fromIntegral i]
  | otherwise = let (q, r) = quotRem i 256 in split256 q ++ [fromIntegral r]

sha1 :: [[Word8]] -> Integer
sha1 str
  = tuple5Fini $ foldl' tuple5Add inits $ map (go . extend . merge) str
  where
    inits = (h0, h1, h2, h3, h4)
    tuple5Fini (a, b, c, d, e)
      = shiftL h0 128 .|. shiftL h1 96 .|. shiftL h2 64 .|. shiftL h3 32 .|. h4
      where
        h0 = fromIntegral a :: Integer
        h1 = fromIntegral b :: Integer
        h2 = fromIntegral c :: Integer
        h3 = fromIntegral d :: Integer
        h4 = fromIntegral e :: Integer
    tuple5Add (a, b, c, d, e) (a', b', c', d', e')
      = (a + a', b + b', c + c', d + d', e + e')
    merge :: [Word8] -> [Word32]
    merge []
      = []
    merge (x : x' : x'' : x''' : xs)
      = foldl' ((. fromIntegral) . (+) . (256 *) . fromIntegral)
        0 [x, x', x'', x'''] : merge xs
    extend xs
      = let res = xs ++
              extend' 16 (drop 13 res) (drop 8 res) (drop 2 res) res
        in res
    extend' 80 _ _ _ _
      = []
    extend' i (x2 : x2s) (x7 : x7s) (x13 : x13s) (x15 : x15s)
      = rotateL (x2 `xor` x7 `xor` x13 `xor` x15) 1 : 
        extend' (i + 1) x2s x7s x13s x15s
    go = go' 0 (h0, h1, h2, h3, h4)
    go' 80 res _
      = res
    go' i (a, b, c, d, e) (x : xs)
      = go' (i + 1) (a', b', c', d', e') xs
      where
        e' = d
        d' = c
        c' = rotateL b 30
        b' = a
        a' = rotateL a 5 + f + e + k + x
        (f, k)
          | i <= 19 = ((b .&. c) .|. (complement b .&. d), c0)
          | i <= 39 = (b `xor` c `xor` d, c1)
          | i <= 59 = ((b .&. c) .|. (b .&. d) .|. (c .&. d), c2)
          | i <= 79 = (b `xor` c `xor` d, c3)
