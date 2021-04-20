data Point = Point
  { x :: Double
  , y :: Double
  }
  deriving Show

zero :: Point
zero = Point {x = 0, y = 0}

xPlusThree :: Point -> Point

