data Point = Point
  { x :: Double
  , y :: Double
  }
  deriving Show

zero :: Point
zero = Point {x = 0, y = 0}

xPlusThree :: Point -> Point
xPlusThree point
  = point {y = y point + 3}

xPlusThree' :: Point -> Point
xPlusThree' (Point x y)
  = Point x (y + 3)
