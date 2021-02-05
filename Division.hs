import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Foldable


--------------------------------------------------------------------------------
-- Non-Restoring Division
--------------------------------------------------------------------------------

sigBit :: Int
sigBit = 63

division :: Word -> Word -> (Word, Word)
division dividend divisor = execState (do
  forM_ [0..sigBit] $ \_ -> do
    (dvd, rmd) <- get
    let (rmd', dvd') = twoShiftL rmd dvd
    if testBit rmd sigBit
      then assign dvd' (rmd' + divisor)
      else assign dvd' (rmd' - divisor)
  (dvd, rmd) <- get
  if testBit rmd sigBit
    then put (dvd, rmd + divisor)
    else return ()
  ) (dividend, 0)
  where
    twoShiftL a b
      = (shiftL a 1 + if testBit b sigBit then 1 else 0, shiftL b 1)
    assign dvd rmd 
      = put (dvd + if testBit rmd sigBit then 0 else 1, rmd)


--------------------------------------------------------------------------------
-- Unsigned Multiplication
--------------------------------------------------------------------------------

multiplication :: Word -> Word -> (Word, Word)
multiplication multiplicand multiplier = execState (do
  forM_ [0..sigBit] $ \_ -> do
    (pdt, mpc) <- get
    if testBit mpc 0
      then put $ twoShiftR (pdt + multiplier) mpc
      else put $ twoShiftR pdt mpc
  ) (0, multiplicand)
  where
    twoShiftR a b
      = (shiftR a 1, shiftR b 1 + if testBit a 0 then bit sigBit else 0)
