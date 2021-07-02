import           Data.Char (isAlpha, isSpace, isDigit)
import           Data.List (intercalate)
import           Text.Read (readMaybe)

data JSON 
  = JBool Bool
  | JNum Double
  | JStr String
  | JList [JSON]
  | JObj [(String, JSON)]

data JToken
  = JTLSB
  | JTRSB
  | JTLCB
  | JTRCB
  | JTColon
  | JTComma
  | JTBool Bool
  | JTNum Double
  | JTStr String
  deriving Show

data JKey
  = KeyDisabled
  | Key (Maybe String)

instance Show JSON where
  show (JBool b)  = if b then "true" else "false"
  show (JNum n)   = show n
  show (JStr str) = show str
  show (JList xs) = "[" ++ drop 2 (show' xs) ++ "]"
    where
      show' []       = ""
      show' (x : xs) = ", " ++ show x ++ show' xs
  show (JObj o)   = "{" ++ drop 2 (show' o) ++ "}"
    where
      show' []           = ""
      show' ((k, v) : o) = ", " ++ show k ++ " : " ++ show v ++ show' o

tokenise :: String -> [JToken]
tokenise []          = []
tokenise ('[' : str) = JTLSB : tokenise str
tokenise (']' : str) = JTRSB : tokenise str
tokenise ('{' : str) = JTLCB : tokenise str
tokenise ('}' : str) = JTRCB : tokenise str
tokenise (':' : str) = JTColon : tokenise str
tokenise (',' : str) = JTComma : tokenise str
tokenise (ch : str)
  | isSpace ch = tokenise str
tokenise str
  | Just n <- readMaybe token :: Maybe Double   = JTNum n : tokenise rest
  | token == "true"                             = JTBool True : tokenise rest
  | token == "false"                            = JTBool False : tokenise rest
  | Just s <- readMaybe token :: Maybe String = JTStr s : tokenise rest
  | otherwise                                   = error "Tokenise Error"
  where
    (token, rest) = break (`elem` "{}[],: \n\t\v\r\f") str

parse :: [JToken] -> JSON
parse ts 
  | (JList [res], []) <- go ts KeyDisabled = res
  | otherwise                              = error "Extra tokens!"
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
    go (JTRCB : _) KeyDisabled = error "Mismatched brackets!"
    go (JTRSB : _) (Key _)     = error "Mismatched brackets!"

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
        (lObj, ts')        = go ts (Key Nothing)
        (JObj rObj, ts'') = go ts' (Key Nothing)

    -- Square braces
    go (JTRSB : ts) _                = (JList [], ts)
    go (JTLSB : ts) KeyDisabled      = (JList (lList : rList), ts'')
      where
        (lList, ts')        = go ts KeyDisabled
        (JList rList, ts'') = go ts' KeyDisabled
    go (JTLSB : ts) (Key (Just str)) = (JObj ((str, lList) : rObj), ts'')
      where
        (lList, ts')      = go ts KeyDisabled
        (JObj rObj, ts'') = go ts' (Key Nothing)

    -- Literals
    go (JTComma : ts) flag             = go ts flag
    go (JTStr str : ts) KeyDisabled    = (JList (JStr str : rem), ts')
      where
        (JList rem, ts') = go ts KeyDisabled
    go (JTBool b : ts) KeyDisabled     = (JList (JBool b : rem), ts')
      where
        (JList rem, ts') = go ts KeyDisabled
    go (JTNum n : ts) KeyDisabled      = (JList (JNum n : rem), ts')
      where
        (JList rem, ts') = go ts KeyDisabled
    go (JTStr str : ts) (Key (Just s)) = (JObj ((s, JStr str) : rem), ts')
      where
        (JObj rem, ts') = go ts (Key Nothing)
    go (JTBool b : ts) (Key (Just s))  = (JObj ((s, JBool b) : rem), ts')
      where
        (JObj rem, ts') = go ts (Key Nothing)
    go (JTNum n : ts) (Key (Just s))   = (JObj ((s, JNum n) : rem), ts')
      where
        (JObj rem, ts') = go ts (Key Nothing)

    -- Valid key
    go (JTStr str : JTColon : ts) (Key Nothing) = go ts (Key $ Just str)

    -- Invalid key
    go bruh (Key Nothing) = error $ "Invalid key!" ++ show bruh

main :: IO ()
main = do
  undefined

json1 :: JSON
json1 = JList [JNum 3.1, JBool False, JObj [("1", JStr "114514")]]

json2 :: JSON
json2 = JObj [("prime", json1), ("foo", JBool True)]

txt1 :: String
txt1 = "[3.1, false, {\"1\" : 114514}]"

txt2 :: String
txt2 = "{\"prime\" : [3.1, false, {\"1\" : \"114514\"}], \"foo\" : true}"
