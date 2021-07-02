import           Data.Char (isAlpha, isSpace, isDigit)
import           Data.List (intercalate)
import           Text.Read (readMaybe)


-- |
-- The JSON data structure.
--
data JSON
  = JBool Bool
  | JNum Double
  | JStr String
  | JNull
  | JList [JSON]
  | JObj [(String, JSON)]


-- |
-- Tokens for JSON strings.
--
data JToken
  = JTLSB
  | JTRSB
  | JTLCB
  | JTRCB
  | JTColon
  | JTComma
  | JTNull
  | JTBool Bool
  | JTNum Double
  | JTStr String
  deriving (Eq, Show)


-- |
-- A helper data structure used in the parse function.
-- @KeyDisabled@ means the parser is not immediately in an object; otherwise
-- the @Maybe String@ represents the latest key, if exists.
--
data JKey
  = KeyDisabled
  | Key (Maybe String)


instance Show JSON where
  show (JBool b)  = if b then "true" else "false"
  show (JNum n)   = show n
  show (JStr str) = show str
  show JNull      = "null"
  show (JList xs) = "[" ++ drop 2 (show' xs) ++ "]"
    where
      show' []       = ""
      show' (x : xs) = ", " ++ show x ++ show' xs
  show (JObj o)   = "{" ++ drop 2 (show' o) ++ "}"
    where
      show' []           = ""
      show' ((k, v) : o) = ", " ++ show k ++ " : " ++ show v ++ show' o


tokenise :: String -> [JToken]
-- Base Case
tokenise [] = []

-- Symbols
tokenise ('[' : str) = JTLSB : tokenise str
tokenise (']' : str) = JTRSB : tokenise str
tokenise ('{' : str) = JTLCB : tokenise str
tokenise ('}' : str) = JTRCB : tokenise str
tokenise (':' : str) = JTColon : tokenise str
tokenise (',' : str) = JTComma : tokenise str
tokenise (ch : str)
  | isSpace ch = tokenise str

-- String
tokenise ('\"' : str)
  | not (null rest) = JTStr token : tokenise (tail rest)
  where
    (token, rest) = break (== '\"') str

-- Number/Bool
tokenise str
  | Just n <- readMaybe token :: Maybe Double = JTNum n : tokenise rest
  -- Note that in JSON booleans are in all lowercase
  | token == "true"                           = JTBool True : tokenise rest
  | token == "false"                          = JTBool False : tokenise rest
  | token == "null"                           = JTNull : tokenise rest
  | otherwise                                 = error "Tokenise Error!"
  where
    (token, rest) = break (`elem` "{}[],: \n\t\v\r\f") str


-- |
-- Return the number of left brackets minus the number of right ones.
-- Doesn't care about bracket types (sqaure or curly).
--
netBrace :: [JToken] -> Int
netBrace ts = sum $ braceCount <$> ts
  where
    braceCount JTLCB = 1
    braceCount JTLSB = 1
    braceCount JTRCB = -1
    braceCount JTRSB = -1
    braceCount _     = 0


parse :: [JToken] -> JSON
parse [] = error "No Tokens!"
parse ts
  | netBrace ts > 0                       = error "Missing closing brackets!"
  | netBrace ts < 0                       = error "Duplicate closing brackets!"
  -- The helper @go@ wraps the tokens in a list, thus if the result is
  -- singleton list, we take the sole element; if it has more than one element,
  -- there are extra tokens.
  | (JList [res], _) <- go ts KeyDisabled = res
  | otherwise                             = error "Extra tokens!"
  where
    -- Invalid commas & colons
    go (JTLSB : JTComma : _) _   = error "Comma following a bracket!"
    go (JTLCB : JTComma : _) _   = error "Comma following a bracket!"
    go (JTComma : JTRSB : _) _   = error "Extra comma!"
    go (JTComma : JTRCB : _) _   = error "Extra comma!"
    go (JTColon : JTComma : _) _ = error "Extra comma!"
    go (JTComma : JTComma : _) _ = error "Extra comma!"
    go (JTColon : _) KeyDisabled = error "Unexpected colon in list!"
    go (JTColon : _) _           = error "Extra colon!"

    -- Invalid right braces
    -- The JKey argument is determined by the left brace, thus if it does
    -- not match with the current right brace token, there's an error
    go (JTRCB : _) KeyDisabled = error "Mismatched brackets!"
    go (JTRSB : _) (Key _)     = error "Mismatched brackets!"
    -- A right brace must be followed by another right brace or a comma
    go (JTRCB : token : ts) _
      | token /= JTRCB && token /= JTRSB && token /= JTComma
        = error "Mismatched brackets!"
    go (JTRSB : token : ts) _
      | token /= JTRCB && token /= JTRSB && token /= JTComma
        = error "Mismatched brackets!"

    -- Base case
    go [] _ = (JList [], [])

    -- Curly braces
    go (JTRCB : ts) _              = (JObj [], ts)
    go (JTLCB : ts) KeyDisabled    = (JList (lObj : rList), ts'')
      where
        (lObj, ts')         = go ts (Key Nothing)
        (JList rList, ts'') = go ts' KeyDisabled
    go (JTLCB : ts) (Key (Just s)) = (JObj ((s, lObj) : rObj), ts'')
      where
        (lObj, ts')       = go ts (Key Nothing)
        (JObj rObj, ts'') = go ts' (Key Nothing)

    -- Square braces
    go (JTRSB : ts) _                = (JList [], ts)
    go (JTLSB : ts) KeyDisabled      = (JList (lList : rList), ts'')
      where
        (lList, ts')        = go ts KeyDisabled
        (JList rList, ts'') = go ts' KeyDisabled
    go (JTLSB : ts) (Key (Just s)) = (JObj ((s, lList) : rObj), ts'')
      where
        (lList, ts')      = go ts KeyDisabled
        (JObj rObj, ts'') = go ts' (Key Nothing)

    -- Literals
    go (JTComma : ts) flag = go ts flag
    go (token : ts) KeyDisabled
      | JTStr str <- token = (JList (JStr str : rem), ts')
      | JTBool b  <- token = (JList (JBool b : rem), ts')
      | JTNum n   <- token = (JList (JNum n : rem), ts')
      | JTNull    <- token = (JList (JNull : rem), ts')
      where
        (JList rem, ts') = go ts KeyDisabled
    go (token : ts) (Key (Just s))
      | JTStr str <- token = (JObj ((s, JStr str) : rem), ts')
      | JTBool b  <- token = (JObj ((s, JBool b) : rem), ts')
      | JTNum n   <- token = (JObj ((s, JNum n) : rem), ts')
      | JTNull    <- token = (JObj ((s, JNull) : rem), ts')
      where
        (JObj rem, ts') = go ts (Key Nothing)

    -- Record keys
    go (JTStr str : JTColon : ts) (Key Nothing) = go ts (Key $ Just str)
    go _ (Key Nothing)                          = error "Invalid key!"


-- | Convert a String to JSON, then print it out.
-- The output should be the same as the input.
--
main :: IO ()
main = do
  str <- getLine
  print $ parse $ tokenise str

json1 :: JSON
json1 = JList [JNum 3.1, JBool False, JObj [("1", JStr "114514")]]

json2 :: JSON
json2 = JObj [("prime", json1), ("foo", JBool True)]

json3 :: JSON
json3 = JObj [("first", json2), ("second", JNum 3.2), ("third", json1)]

txt1 :: String
txt1 = "[3.1, false, {\"1\" : 114514}]"

txt2 :: String
txt2 = "{\"prime\" : [3.1, false, {\"1\" : \"114514\"}], \"foo\" : true}"

txt3 :: String
txt3 = "{\"first\" : {\"prime\" : [3.1, false, {\"1\" : \"114514\"}], \
       \\"foo\" : true}, \"second\" : 3.2, \"third\" : [3.1, false, {\"1\" : \
       \\"114514\"}]}"
