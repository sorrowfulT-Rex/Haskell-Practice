import Data.Char
import Control.Monad
import Control.Monad.Trans.Class
import Data.Maybe 
import Text.Read hiding (lift, get)
-- import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

-- Without Monad Transformer

getPassphrase :: IO (Maybe String)
getPassphrase = do
  s <- getLine
  return $ if isValid s
    then Just s
    else Nothing

isValid :: String -> Bool
isValid s
  = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

askPassphrase :: IO ()
askPassphrase = do
  putStrLn "Insert your new passphrase:"
  maybe_value <- getPassphrase
  case maybe_value of
    Just value -> putStrLn "Storing in database..."
    Nothing    -> putStrLn "Passphrase invalid."

-- MaybeT
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => Applicative (MaybeT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ do
    mb <- runMaybeT x
    case mb of
      Nothing -> return Nothing
      Just e  -> runMaybeT $ f e

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

getPassphrase' :: MaybeT IO String
getPassphrase' = do
  s <- lift getLine
  if isValid s
    then return s
    else MaybeT $ return Nothing

askPassphrase' :: IO ()
askPassphrase' = void $ runMaybeT $ do
  lift $ putStrLn "Insert your new passphrase:"
  value <- getPassphrase'
  lift $ putStrLn "Storing in database..."

-- ListT
newtype ListT m a = ListT { runListT :: m [a] }

instance Monad m => Functor (ListT m) where
  fmap = liftM

instance Monad m => Applicative (ListT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return  = ListT . return . return
  x >>= f = ListT $ do
    list <- runListT x
    let foo = runListT . f
    concat <$> mapM foo list

instance MonadTrans ListT where
  lift = ListT . fmap return

listAdder :: IO [Int]
listAdder = do
  l1 <- getLine
  adder l1 <$> getLine

adder :: String -> String -> [Int]
adder l1 l2 = do
  x <- l1
  y <- l2
  return $ read [x] + read [y]

listAdder' :: IO [Int]
listAdder' = runListT $ do
  x <- ListT getLine
  y <- ListT getLine
  return $ read [x] + read [y]

-- Either Transformer
isValid' :: String -> Either String String
isValid' s
  | length s <= 8             = Left "Too short!"
  | not (any isAlpha s)       = Left "Must contain a letter!"
  | not (any isNumber s)      = Left "Must contain a number!"
  | not (any isPunctuation s) = Left "Must contain a punctuation mark!"
  | otherwise                 = Right s

getPwdT :: ExceptT String IO String
getPwdT = do
  s <- lift getLine
  ExceptT $ return $ isValid' s

askPwdT :: ExceptT String IO String
askPwdT = do
  lift $ putStrLn "Insert your new password:"
  pwd <- getPwdT
  lift $ putStrLn "Storing in database..." >> return pwd

askPwd :: IO ()
askPwd = do 
  r <- runExceptT askPwdT
  putStrLn $ case r of
    Left error -> error
    Right pwd  -> "The password is " ++ pwd ++ " (why would you show it?!"

-- Either but without Transformer
askPwd' :: IO ()
askPwd' = do
  putStrLn "Insert your new password:"
  either <- isValid' <$> getLine
  case either of
    Left error -> putStrLn error
    Right _    -> putStrLn "Storing in database..."

-- Identity Transformer
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure  = return
  (<*>) = ap

newtype IdentityT m a = IdentityT {runIdentityT :: m a}

instance Monad m => Functor (IdentityT m) where
  fmap = liftM

instance Monad m => Applicative (IdentityT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (IdentityT m) where
  return  = IdentityT . return
  x >>= f = IdentityT $ do
    x' <- runIdentityT x
    runIdentityT $ f x'

instance MonadTrans IdentityT where
  lift = IdentityT

trivialAdd :: IO ()
trivialAdd = runIdentityT $ do
  x <- lift readLn
  y <- lift readLn
  lift $ print $ x + y

-- Mixed Transformer
io1, io2 :: MaybeT (ExceptT String IO) Int
io1 = MaybeT $ ExceptT $ return $ Right $ Just 1
io2 = MaybeT $ ExceptT $ return $ Right $ Just 2

-- Take two int inputs. If invalid, Left; if < 0, Right Nothing.
fooAdd :: IO (Either String (Maybe ()))
fooAdd = runExceptT $ runMaybeT $ do
  x <- lift' $ parser <$> getLine
  y <- lift' $ parser <$> getLine
  lift . lift $ print $ x + y

parser :: String -> Either String (Maybe Int)
parser str = runMaybeT $ do
  x <- lift $ readEither str :: MaybeT (Either String) Int
  if x < 0
    then MaybeT $ Right Nothing
    else lift $ Right x 

lift' :: Monad m => m (Either String (Maybe Int)) -> MaybeT (ExceptT String m) Int
lift' x
  = MaybeT $ ExceptT x

bruh :: StateT Int Maybe ()
bruh = do
  x <- get
  guard (x `mod` 3 /= 0)
  put x
