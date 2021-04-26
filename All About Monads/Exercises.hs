module Exercise where

import           Control.Monad
import           Data.Maybe

-- 1

type Sheep = Int

father :: Sheep -> Maybe Sheep
father i
  | even i    = Just $ i `div` 2
  | otherwise = Nothing

mother :: Sheep -> Maybe Sheep
mother i
  | odd i      = Just $ i `div` 2
  | otherwise  = Nothing

-- 1.1
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather = mother >=> father >=> father

-- 1.2
parent :: Sheep -> Maybe Sheep
parent s = father s `mplus` mother s

grandParent :: Sheep -> Maybe Sheep
grandParent s = (father s >>= father) `mplus`
                (father s >>= mother) `mplus`
                (mother s >>= father) `mplus`
                (mother s >>= mother)

-- 1.3
parent' :: Sheep -> [Sheep]
parent' s = catMaybes [father s, mother s]

grandParent' :: Sheep -> [Sheep]
grandParent' s = catMaybes [ father s >>= father
                           , father s >>= mother
                           , mother s >>= father
                           , mother s >>= mother
                           ]

-- 1.4
maybeToMonadPlus :: MonadPlus m => Maybe a -> m a
maybeToMonadPlus Nothing  = mzero
maybeToMonadPlus (Just x) = return x

parentMP :: MonadPlus m => Sheep -> m Sheep
parentMP s = foldl mplus mzero $ maybeToMonadPlus <$> [father s, mother s]

grandParentMP :: MonadPlus m => Sheep -> m Sheep
grandParentMP s = msum $ maybeToMonadPlus <$> [ father s >>= father
                                              , father s >>= mother
                                              , mother s >>= father
                                              , mother s >>= mother
                                              ]
