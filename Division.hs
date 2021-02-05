import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Foldable


--------------------------------------------------------------------------------
-- Non-Restoring Division
--------------------------------------------------------------------------------

sigBit :: Int
sigBit = 63

division :: Int -> Int -> (Int, Int)
division dividend divisor = execState (do
  forM_ [0..sigBit] $ \_ -> do
    (dvd, rmd) <- get
    let (rmd', dvd') = twoShift rmd dvd
    if testBit rmd sigBit
      then assign dvd' (rmd' + divisor)
      else assign dvd' (rmd' - divisor)
  (dvd, rmd) <- get
  if testBit rmd sigBit
    then put (dvd, rmd + divisor)
    else return ()
  ) (dividend, 0)
  where
    twoShift a b
      = (shiftL a 1 + if testBit b sigBit then 1 else 0, shiftL b 1)
    assign dvd rmd 
      = put (dvd + if testBit rmd sigBit then 0 else 1, rmd)
