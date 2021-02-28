module PointFree where

import           Control.Monad
import           Data.List
import           Data.Maybe

data Function = Fun Head Body
data Head     = Name String [String]
data Body     = Apply Body Body | Arg String | Dummy
  deriving Eq
data Token    = Literal String | Operator String | LeftB | RightB | BackTick
  deriving (Eq, Show)
data Asso     = L | R | N
  deriving Eq

instance Show Function where
  show (Fun h b)
    = show h ++ " = " ++ show b

instance Show Head where
  show (Name str args)
    = str ++ foldl' ((. (' ' :)) . (++)) "" args

instance Show Body where
  show (Arg str)
    = str
  show (Apply b b')
    = "(" ++ show b ++ ") (" ++ show b' ++ ")"
  show Dummy
    = "Dummy"

apply :: String -> Body -> Body -> Body
apply ";" b b'
  = Apply b b'
apply o Dummy Dummy
  = Arg o
apply o Dummy b
  = Apply (Apply (Arg "flip") (Arg o)) b
apply o b Dummy
  = Apply (Arg o) b
apply o b b'
  = Apply (Apply (Arg o) b) b'

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- Break a list into list of lists on predicate
split :: (a -> Bool) -> [a] -> [[a]]
split _ []
  = [[]]
split f (a : as)
  | f a       = [] : (r : rs)
  | otherwise = (a : r) : rs
  where
    (r : rs) = split f as

-- Break a list into list of lists on given elements while keeping the elements
splitProper :: Eq a => [a] -> [a] -> [[a]]
splitProper _ []
  = [[]]
splitProper keys (a : as)
  | elem a keys = [] : [a] : (r : rs)
  | otherwise   = (a : r) : rs
  where
    (r : rs) = splitProper keys as

-- Break a String on '=' but not separating infix operators containing '='
breakOnEq :: String -> (String, String)
breakOnEq str
  = break' str True
  where
    break' "" _
      = ("", "")
    break' az@(')' : as) False
      = break' az True
    break' az@('(' : as) True
      = break' az False
    break' (a : as) flag
      | a == '=' && flag = ([], as)
      | otherwise        = (a : f, s)
      where
        (f, s) = break' as flag

precTable :: [(String, Int)]
precTable
  = [(".", 9), ("^", 8), ("^^", 8), ("**", 8), ("*", 7), ("/", 7), ("`rem`", 7),
     ("`quot`", 7), ("`div`", 7), ("`mod`", 7), ("+", 6), ("-", 6), (":", 5),
     ("==", 4), (">=", 4), ("<=", 4), (">", 4), ("<", 4), ("/=", 4), ("&&", 3),
     ("||", 2), (">>", 1), (">>=", 1), ("=<<", 1), ("$", 0), ("$!", 0), 
     ("`seq`", 0), ("#", -1), (";", 114514)
    ]

assoTable :: [(String, Asso)]
assoTable
  = [(".", R), ("^", R), ("^^", R), ("**", R), ("*", L), ("/", L), ("`rem`", L),
     ("`quot`", L), ("`div`", L), ("`mod`", L), ("+", L), ("-", L), (":", R),
     ("==", N), (">=", N), ("<=", N), (">", N), ("<", N), ("/=", N), ("&&", R),
     ("||", R), (">>", L), (">>=", L), ("=<<", R), ("$", R), ("$!", R), 
     ("`seq`", R)
    ]

yuusen :: String -> String -> Maybe Bool
yuusen o o'
  | isNothing lo  = Just $ isJust lo' || (asso /= L)
  | isNothing lo' = Just False
  | flo < flo'    = Just False
  | flo > flo'    = Just True
  | asso == L     = Just False
  | asso == R     = Just True
  | otherwise     = Nothing
  where
    lo   = lookup o precTable
    lo'  = lookup o' precTable
    flo  = fromJust lo
    flo' = fromJust lo'
    asso = fromMaybe L $ lookup o assoTable

operators :: [Char]
operators = "+-*/<!>=:&|."


--------------------------------------------------------------------------------
-- Parse Functions
--------------------------------------------------------------------------------

-- Tokenise a String
-- tokenise :: String -> [Token]
tokenise str
  = map tokenise' (concat $ map words $ splitProper ("()" ++ operators) str)
  where
    tokenise' "("
      = LeftB
    tokenise' ")"
      = RightB
    tokenise' "`"
      = BackTick
    tokenise' str
      | head str `elem` operators = Operator str
      | otherwise                 = Literal str

-- Combine Tokens into a Function Body
toBody :: [Token] -> Maybe Body
toBody ts
  = fst <$> toBody' ts ["#"] [] False
  where
    toBody' tz@(Operator o : ts) ["#"] [] flag
      = toBody' tz ["#"] [Dummy] flag
    toBody' tz@(Literal l : ts) sanshi sansuu flag
      | flag      = toBody' (Operator ";" : tz) sanshi sansuu False
      | otherwise = toBody' ts sanshi (Arg l : sansuu) True
    toBody' (LeftB : RightB : ts) sanshi sansuu flag
      = toBody' (Literal "()" : ts) sanshi sansuu flag
    toBody' tz@(LeftB : ts) sanshi sansuu flag
      | flag             = toBody' (Operator ";" : tz) sanshi sansuu False
      | isNothing inside = Nothing
      | otherwise        = toBody' r sanshi (b : sansuu) True
      where
        inside = toBody' ts ["#"] [] False
        (b, r) = fromJust inside
    toBody' (RightB : ts) sanshi sansuu flag
      | null ts         = liftM2 (,) inside (Just ts)
      | tH == LeftB     = liftM2 (,) inside (Just $ Operator ";" : ts)
      | Literal _ <- tH = liftM2 (,) inside (Just $ Operator ";" : ts)
      | otherwise       = liftM2 (,) inside (Just ts)
      where
        tH     = head ts
        inside = fst <$> toBody' [] sanshi sansuu flag
    toBody' (Operator o : Operator o' : ts) sanshi sansuu _
      = toBody' (Operator (o ++ o') : ts) sanshi sansuu False
    toBody' tz@(Operator o : ts) sanshi sansuu _
      | null sanshi = toBody' ts (o : sanshi) sansuu False
      | highPrec    = toBody' ts (o : sanshi) sansuu False
      | invalid     = Nothing
      | otherwise   = toBody' tz (tail sanshi) (apply o' s' s : r) False
      where
        o'           = head sanshi
        ys           = o `yuusen` o'
        highPrec     = ys == Just True
        invalid      = isNothing ys || null sansuu || null (tail sansuu)
        (s : s' : r) = sansuu
    toBody' [] sanshi sansuu False
      = toBody' [] sanshi (Dummy : sansuu) True
    toBody' [] (o : sanshi) tachi@(s : s' : sansuu) True
      | o == "#"  = error $ show (s : s' : sansuu)
      | otherwise = toBody' [] sanshi (apply o s' s : sansuu) True
    toBody' [] ["#"] [s] _
      = Just (s, [])
    toBody' _ _ _ _
      = Nothing

-- parse a String input into a function
parse :: String -> Maybe Function
parse str
  | null hs     = Nothing
  | isNothing b = Nothing
  | otherwise   = Just $ Fun h $ fromJust b
  where
    (hRaw, bRaw) = breakOnEq str
    hs           = words hRaw
    h            = Name (head hs) (tail hs)
    b            = toBody $ tokenise bRaw

-- -- Find explicit arguments
-- findExplicit :: Function -> [Arg]
-- findExplicit 
