
class Gar a where

instance Gar Int where

-- Allowed
instance Gar (Maybe a) where

-- NOT Allowed because the type contructor must contain type variables only;
-- no nesting contructors.
-- Need FlexibleInstances
-- instance Gar (Maybe Int) where
