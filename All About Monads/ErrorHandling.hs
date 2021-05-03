-- This is an introduction on basic error-handling in Haskell.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This is an introduction on basic error-handling in Haskell.

import           Control.Exception (Exception, SomeException, catch, throw,)
import           Control.Monad (liftM2, void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
  (Except, ExceptT, catchE, throwE, runExcept, runExceptT)
import           Data.Char (digitToInt, isDigit)
import           Data.List (foldl')
import           Data.Maybe (mapMaybe)
import           Data.Text as T (Text, concat, pack, replace, singleton, unpack)


--------------------------------------------------------------------------------
--  0. Most Basic Pure Error
--------------------------------------------------------------------------------

-- Consider the following function that parses a character into an integer.
--
toInt :: Char -> Int
toInt ch
  | isDigit ch = digitToInt ch

  -- toInt '3'
    -- 3
  -- toInt 's'
    -- Exception: ...: Non-exhaustive patterns in function toInt

-- Apparantly the previous function cannot handle the case when the input is
-- invalid. We can add a custom error message to notify the user at such cases.
--
toIntWithError :: Char -> Int
toIntWithError ch
  | isDigit ch = digitToInt ch
  | otherwise  = error $ "Invalid input " ++ ch : "!"

  -- toIntWithError '3'
    -- 3
  -- toIntWithError 's'
    -- Exception: Invalid input s!
-- When error is called, the computation is terminated and the error message is
-- printed.

-- And we can use this function to turn a @String@ into an @Integer@.
--
parseInt :: String -> Integer
parseInt = foldl (\a b -> 10 * a + b) 0 . map (toInteger . toIntWithError)

  -- parseInt "123"
    -- 123
  -- parseInt "123a"
    -- Exception: Invalid input a!
  -- parseInt ""
    -- 0
-- Of course the third example is a glitch; we (normally) don't want an empty
-- string to be parsed into zero. But let's ignore this small issue as it can'
-- be easily fixed. Another problem is that it does not recognise the negation
-- symbol, but we will ignore it in this example as well.

-- The next example, however, shows how lazy evaluation affects error-handling:

  -- parseInt "123abc"
    -- Exception: Invalid input c!
-- Well well well, we would assume that the exception should happen at 'a', the
-- first non-digit character in the @String@. However it does not work that way
-- since the foldl function is lazy. Let's look in close detail on how
-- @parseInt@ is computed in this example:
  -- -- let f = \a b -> 10 * a + b; g = toInteger . toIntWithError
  --   parseInt "123abc"
  -- = foldl f 0 "123abc"
  -- = foldl f (10 * 0 + g '1') "23abc"
  -- = foldl f (10 * (10 * 0 + g '1') + g '2') "3abc"
  -- = foldl f (10 * (10 * (10 * 0 + g '1') + g '2') + g '3') "abc"
  -- = ...
  -- = foldl f (10 * (...) + g 'c') []
  -- = 10 * (...) + g 'c'

-- In Haskell, the second operator of (+) is evaluated first, thus in this case
-- it computes @g 'c'@, resulting in an error and terminates the program.
-- Although @g 'a'@ is "encountered" first, it is never evaluated.
-- This can be quite a problem, especially if the invalid character occurs at
-- the beginning of a very long message; we would expect the parser to
-- immediately halt with the error when the invalid character is encountered,
-- rather than having to go through the entire string.

-- To ensure that the first error will be reported, we can use the strict 
-- verison of @foldl@, namely @foldl'@ (in Data.List), so that the accumulator
-- is always calculated along the way:
--
parseInt' :: String -> Integer
parseInt' = foldl' (\a b -> 10 * a + b) 0 . map (toInteger . toIntWithError)

  -- parseInt' "123abc"
    -- Exception: Invalid input a!

-- We will return to this issue shortly in the next section.

-- Now we expand the error message a little bit, showing also the location of
-- the error. Note that we used the bang notation "!" to make v, the accumuator
-- strict (with language extension BangPatterns). Otherwise we will have the
-- same issue mentioned above where the last error is reported, not the first.
--
parseIntWithLocation :: String -> Integer
parseIntWithLocation s
  = go s 0 0
  where
    go [] !v _       = v
    go (c : cs) !v i = go cs (v * 10 + charToInt c i) (i + 1)
    charToInt ch i
      | isDigit ch = toInteger $ digitToInt ch
      | otherwise  = error $ "At index " ++ show i ++
                             ": Invalid character " ++ ch : "!"

  -- parseIntWithLocation "114514"
    -- 114514
  -- parseIntWithLocation "1a2b"
    -- Exception: At index 1: Invalid character a!


--------------------------------------------------------------------------------
--  1. Custom Error Types
--------------------------------------------------------------------------------

-- So far we represent the error with a @String@. However an arguably better way
-- this is to create a type that records the location where the error occurs, 
-- the type of the error, and the relevant character.
--
data ParseError = PE
  { location :: Int
  , char     :: Char
  , reason   :: Text
  }

-- If you haven't seen @Text@, that's just a @String@-like type with more
-- efficient manipulations such as substring replacement (in Data.Text).

defaultPE :: ParseError
defaultPE = PE { location = -1
                , char = '\0'
                , reason = "Index at {i}: Invalid character {c}!"
               }

instance Show ParseError where
  show PE {location = i, char = c, reason = s}
    = unpack $ replace "{c}" (singleton c) $ replace "{i}" (pack $ show i) s

  -- :set -XOverloadedStrings 
  -- PE 1 's' "Index at {i}: Invalid character {c}!"
    -- Index at 1: Invalid character s!

-- Now we want to make this @ParseError@ type "throwable", in other words, we
-- want the program to be able to "recognise" this type as an error type.
--
instance Exception ParseError

-- The class 'Exception' is exported from Control.Exception.
-- Note that the minimum completion definition requires no method at all!
-- However the instance type must also be an instance of Show, this is because
-- when the error is thrown, it displays its string representation.

-- To throw a custom error, we use the function @throw@ (in Control.Exception).
--
parseIntWithParseError :: String -> Integer
parseIntWithParseError s
  = go s 0 0
  where
    go [] !v _       = v
    go (c : cs) !v i = go cs (v * 10 + charToInt c i) (i + 1)
    charToInt ch i
      | isDigit ch = toInteger $ digitToInt ch
      | otherwise  = throw defaultPE {location = i, char = ch}

  -- parseIntWithParseError "1919810"
    -- 1919810
  -- parseIntWithParseError "1919ya10"
    -- Exception: Index at 4: Invalid character y!


--------------------------------------------------------------------------------
--  2. Impure Errors with IO
--------------------------------------------------------------------------------

-- You may have noticed that in the case of pure functions, if you encounters an
-- error, there isn't much you can do to recover from it: the program halts.
-- To be able to "catch" an exception, we need an impure context. The most
-- natural way is to wrap the calculation in the @IO@ monad.
-- It is OK to continue reading this section without any knowledge on monads, 
-- but knowing it can help a lot.

-- The following function reads a line from standard input, parse it into an
-- @Integer@, then print it out.
--
parseIntIO :: IO ()
-- In the case of @IO@, the do block simply executes line by line as in
-- imperative programming. When we get an input, we don't say @raw = getLine@,
-- but @raw <- getLine@
parseIntIO = do
  putStrLn "Please type in a positive integer:"
  -- raw is the @String@ the user types in
  raw <- getLine
  -- We then print out the parsed result
  putStrLn $ "The result is: " ++ show (parseIntWithParseError raw) ++ "."
  putStrLn "Thank you!"

  -- parseIntIO
    -- Please type in a positive integer:
  -- 123
    -- The result is: 123.
    -- Thank you!
  -- parseIntIO
    -- Please type in a positive integer:
  -- 123abc
    -- The result is: *** Exception: Index at 3: Invalid character a!
-- When an invalid input is typed in, the monadic action halts, just like what
-- we have seen in the pure codes.
-- What we want is to "catch" the error. In other words, when an invalid input
-- occurs, we want to do something according to the error, then resume the
-- normal flow of the action. In this particular case, the goal is to print out
-- the error, then continue with the next line (printing out "Thank you!").

-- The key is to use the @catch@ function (in Control.Exception).
-- The type signature of @catch@ is Exception e => IO a -> (e -> IO a) -> IO a,
-- where the first argument is the monadic action that may result an error, the
-- second argument tells the function what to do in the case of an error, and
-- the result is a monadic action that handles such errors.
-- It may be a bit abtract to understand, but with an example this should be
-- nice and clear~
--
parseIntIOCatched :: IO ()
parseIntIOCatched = do
  -- Here @askForInputAndParse@ is the same as the section in @parseIntIO@, but
  -- without the last "Thank you!" phrase.
  -- The second argument of catch is a lambda. The argument @e@ represents an
  -- @ParseError@. When a @ParseError@ is encountered in @askForInputAndParse@,
  -- @catch@ will immediately switch to the lambda, passing the error to the
  -- argument.
  -- In this example, the lambda simply prints out the error. After it finishes
  -- executing, we return to the original flow (which is the next line in the do
  -- block, printing "Thank you!").
  askForInputAndParse `catch` (\e@(PE i c s) -> print e)
  putStrLn "Thank you!"
  where
    askForInputAndParse = do
      putStrLn "Please type in a positive integer:"
      -- raw is the @String@ the user types in
      raw <- getLine
      -- We then print out the parsed result
      putStrLn $ "The result is: " ++ show (parseIntWithParseError raw) ++ "."

  -- parseIntIOCatched 
    -- Please type in a positive integer:
  -- 123
    -- The result is: 123.
    -- Thank you!
  -- parseIntIOCatched 
    -- Please type in a positive integer:
  -- asd4
    -- The result is: Index at 0: Invalid character a!
    -- Thank you!
-- Now "Thank you!" is displayed even in the case of an error, showing that the
-- the error did not halt the full action, thanks to @catch@.
-- However, even in the case of invalid input, "The result is: " is still
-- printed out. This is the lazyness of Haskell standing in our way again: the
-- program does not care about parsing @raw@ until the @String@ literal is
-- shown.

-- To avoid this, we can deliberately make the calculation of 
-- @parseIntWithParseError raw@ happen BEFORE actually printing anything.
--
parseIntIOCatchedStrict :: IO ()
parseIntIOCatchedStrict = do
  askForInputAndParse `catch` (\e@(PE i c s) -> print e)
  putStrLn "Thank you!"
  where
    askForInputAndParse = do
      putStrLn "Please type in a positive integer:"
      -- raw is the @String@ the user types in
      raw <- getLine
      -- We then print out the parsed result
      let n = parseIntWithParseError raw
      -- The @seq@ function forces the calculation of it's first argument before
      -- evaluating the second. It is useful in forcing an order of computation.
      n `seq` putStrLn ("The result is: " ++ show n ++ ".")

  -- parseIntIOCatchedStrict 
    -- Please type in a positive integer:
  -- 12
    -- The result is: 12.
    -- Thank you!
  -- parseIntIOCatchedStrict 
    -- Please type in a positive integer:
  -- 12s
    -- Index at 2: Invalid character s!
    -- Thank you!

-- We can also catch different types of errors by adding a bunch of @catch@s. 
-- Let's demonstrate with an independent small example.
--
foo :: Int -> Int
foo 0 = 0
foo 1 = throw defaultPE  -- Throws an error of type @ParseError@
foo _ = error "Not 0 or 1!"  -- Throws an error using the basic @error@ function

bar :: Int -> IO ()
bar n = do
  print (foo n) `catch`
    (\e@PE {} -> putStrLn "ParseError!") `catch`
    (\(e :: SomeException) -> putStrLn "Basic Error!")
    -- @SomeException@ is exported from Control.Exception  
    -- Requires the extension ScopedTypeVariables to enable type annotations
    -- within the arguments of a lambda
  putStrLn "Thank you!"

  -- bar 0
    -- 0
    -- Thank you!
  -- bar 1
    -- Parse Error!
    -- Thank you!
  -- bar 2
    -- Basic Error!
    -- Thank you!
-- When executing @bar 0@, no error is thrown and it flows normally;
-- When executing @bar 1@, it throws a @ParseError@, and the first @catch@ is
-- triggered;
-- When executing @bar 2@, it throws an error, but not a custom one. Here we
-- use the root of all errors, @SomeException@, to catch it.

-- Note since @SomeException@ is the ancestor of all errors, if we move the
-- @catch@ lambda for @SomeExceptions@ before other errors, all @catch@s after
-- it are unreachable.
--
bar' :: Int -> IO ()
bar' n = do
  print (foo n) `catch`
    (\(e :: SomeException) -> putStrLn "Basic Error!") `catch`
    (\e@PE {} -> putStrLn "ParseError!")
  putStrLn "Thank you!"

  -- bar' 0
    -- 0
    -- Thank you!
  -- bar' 1
    -- Basic Error!
    -- Thank you!
  -- bar' 2
    -- Basic Error!
    -- Thank you!

-- Here the difference occurs at @bar' 1@. Since a @ParseError@ is also a
-- @SomeException@, it falls into the first @catch@ block, ignoring the more
-- specific one that follows.

-- To end this section, let's make a parser that repeatedly asks the user to
-- type in new lines until the input is valid.
--
parseIntIORecursive :: IO ()
parseIntIORecursive
  = do putStrLn "Please type in a positive integer:"
       raw <- getLine
       let n = parseIntWithParseError raw
       n `seq` putStrLn ("The result is: " ++ show n ++ ".")
       putStrLn "Thank you!"
       `catch` (\e@(PE i c s) -> do 
       print e
       parseIntIORecursive)

  -- parseIntIORecursive 
    -- Please type in a positive integer:
  -- qwerty
    -- Index at 0: Invalid character q!
    -- Please type in a positive integer:
  -- 123e
    -- Index at 3: Invalid character e!
    -- Please type in a positive integer:
  -- 543
    -- The result is: 543.
    -- Thank you!


--------------------------------------------------------------------------------
--  3. Maybe, Either & Except
--------------------------------------------------------------------------------

-- So far we've been throwing and catching errors. However, this approach is
-- only available in the @IO@ monad, and this kind of errors cannot be handled
-- in pure codes.
-- Furthermore, we have to be very careful about Haskell's lazyness as it may
-- give us unexpected results.

-- Consider the following function that returns the second element in a list.
--
headOfTail :: [a] -> a
headOfTail (_ : x : _) = x
headOfTail _           = error "List too short!"

-- The following function takes three lists, and returns a list containing the
-- second element in each of the list.
--
headOfTail3 :: [a] -> [a] -> [a] -> [a]
headOfTail3 xs ys zs = map headOfTail [xs, ys, zs]

  -- headOfTail3 [1,2,3] [4,5,6] [7,8,9]
    -- [2,5,8]
  -- headOfTail3 [0] [1,2] [3,4,5]
    -- [*** Exception: List too short!
-- Since the function is pure, the error will always ternimate the function
-- instead of ignoring the list that does not have a second element.

-- If you haven't already, I strongly recommend you to learn monad before
-- continuing the journey.

-- To successfully catch the error, we need an @IO@ context.
--
headOfTail3IO :: Show a => [a] -> [a] -> [a] -> IO ()
headOfTail3IO xs ys zs = do
  let lists = [xs, ys, zs]
  result <- go lists
  print result
  where
    go l = case l of
      []     -> return []
      l : ls -> do rest <- go ls
                   let n = headOfTail l
                   n `seq` return $ n : rest
                   `catch` (\(e :: SomeException) -> go ls)

  -- headOfTail3IO [0] [1,2] [3,4,5]
    -- [2,4]
-- It works, however the code is clunky, the @seq@ looks not nice, and we are
-- deliberately using an @IO@ context for what would be a pure calculation.

-- One of the easiest way to avoid this is to use the @Maybe@ type.
-- 
headOfTailMaybe :: [a] -> Maybe a
headOfTailMaybe (_ : x : _) = Just x
headOfTailMaybe _           = Nothing

-- Now instead of throwing an error, we have well-formd results for any input.

headOfTail3Maybe :: [a] -> [a] -> [a] -> [a]
headOfTail3Maybe xs ys zs = mapMaybe headOfTailMaybe [xs, ys, zs]

-- The function @mapMaybe@ applies it's first argument to each element in the
-- list of the second argument, producing a list of @Maybe@s, then ignore all
-- of the @Nothings@ (in Data.Maybe).
  -- headOfTail3Maybe [1,2,3] [4,5,6] [7,8,9]
    -- [2,5,8]
  -- headOfTail3Maybe [0] [1,2] [3,4,5]
    -- [2,4]
-- As desired.

-- Although I won't further elaborate, this approach of @Maybe@ can be composed
-- neatly in both pure and impure contexts since @Maybe@ is a monad as well.

-- Now let's return to the parser example. A @Maybe@ is not enough in our case
-- because we need to record the information of the error as well. With @Maybe@
-- we only have @Nothing@ denoting an error without further details on it.

-- Here the @Either@ type is our friend:
-- @ Either a b = Left a | Right b @
-- Usually the @Right@ constructor represents a normal result, while @Left@
-- represents an error. In our parser example, we can use the type
-- @Either ParseError Integer@ to represent the result of a parse.
--
parseIntEither :: String -> Either ParseError Integer
parseIntEither s
  = go s 0 0
  where
    go [] v _       = Right v
    go (c : cs) v i = case charToInt c i of
      Left e  -> Left e
      Right n -> go cs (v * 10 + n) (i + 1)
    charToInt ch i
      | isDigit ch = Right $ toInteger $ digitToInt ch
      | otherwise  = Left defaultPE {location = i, char = ch}

  -- parseIntEither "123"
    -- Right 123
  -- parseIntEither "123e"
    -- Left Index at 3: Invalid character e!
-- Now whatever the input is, the function always exits normally since even the
-- "error" is reprensented by a normal value.

parseIntIOEither :: IO ()
parseIntIOEither = do
  putStrLn "Please type in a positive integer:"
  raw <- getLine
  case parseIntEither raw of
    Left e  -> print e
    Right n -> putStrLn $ "The result is: " ++ show n ++ "."
  putStrLn "Thank you!"

  -- parseIntIOEither
    -- Please type in a positive integer:
  -- 123
    -- The result is: 123.
    -- Thank you!
  -- parseIntIOEither
    -- Please type in a positive integer:
  -- err
    -- Index at 0: Invalid character e!
    -- Thank you!

-- Another advantage of this approach is that we no longer have to worry about
-- the problems from lazyness; the correct order of computation is always 
-- maintained.

-- Control.Monad.Trans.Except provides a data type @Except@. Although in its
-- full extent it is a monad transformer, for this section we can simply view
-- its definition as a wrapper around @Either@:
-- @ Except a b = Except (Either a b) @
-- There is also a corresponding @runExcept@ function that simply extracts the 
-- @Either@ wrapped within.
-- With @Except@, instead of writing @Left@ and @Right@, we can use functions
-- such as @throwE@ and @return@, as demonstrated in the next example.
--
parseIntExcept :: String -> Except ParseError Integer
parseIntExcept s
  = go s 0 0
  where
    -- Here @return@ is similar to to @Right@
    go [] v _       = return v
    go (c : cs) v i = do
      -- If @charToInt c i@ is a @Right@, it's value is passed to @n@
      -- If @charToInt c i@ is a @Left@, the execution stops and the error type
      -- (ParseError) is returned
      n <- charToInt c i
      go cs (v * 10 + n) (i + 1)
    charToInt ch i
      | isDigit ch = return $ toInteger $ digitToInt ch
      -- Here @throwE@ is similar to @Left@
      | otherwise  = throwE defaultPE {location = i, char = ch}

  -- parseIntExcept "123"
    -- ExceptT (Identity (Right 123))
  -- parseIntExcept "duh"
    -- ExceptT (Identity (Left Index at 0: Invalid character d!))
-- If we ignore the wrapper (@ExceptT@ and @Identity@), it is essentially the
-- same logic as @Either@.

parseIntIOExcept :: IO ()
parseIntIOExcept = do
  putStrLn "Please type in a positive integer:"
  raw <- getLine
  case runExcept $ parseIntExcept raw of
    Left e  -> print e
    Right n -> putStrLn $ "The result is: " ++ show n ++ "."
  putStrLn "Thank you!"

  -- parseIntIOExcept 
    -- Please type in a positive integer:
  -- 9527
    -- The result is: 9527.
    -- Thank you!
  -- parseIntIOExcept 
    -- Please type in a positive integer:
  -- 3e
    -- Index at 1: Invalid character e!
    -- Thank you!
-- It behaves the same as @parseIntIOEither@, which may make people wonder why
-- is this wrapper around @Either@ necessary. However, it has utility functions
-- for us to make the try-catch sequence clearer. In the next example, we will 
-- see how to use the function @catchE@, also from Control.Monad.Trans.Except.

-- As mentioned before, error-handling using @Either@ or @Except@ is available
-- in pure functions as well. Consider the following pure function that takes
-- in a @String@, attempts to parse it, then returns a @String@ that either says
-- "The result is: ..." or displays the error.
--
parseIntExceptPure :: String -> String
parseIntExceptPure s = result
  where
    Right result = runExcept $ do res <- parseIntExcept s
                                  return $ "The result is: " ++ show res ++ "."
                                  `catchE`
                                  (return . show)

-- To understand the function above, we need to know what @catchE@ do.
-- If the input @String@ is valid, then in the do block, @res@ is the 
-- corresponding @Integer@, and the result is returned normally (note that in
-- the @Either@ monad, @return@ is the same as @Right@).
-- If the input is invalid, then the entire do block produces the corresponding
-- @ParseError@ with the @Left@ constructor (of course it's further wrapped in
-- an @Except@, but the idea is the same). When @catchE@ receives a @Left@ from
-- its first argument, it will treat this @Left@ as an "error", passing it to
-- the second argument. In this example, we simply transform the @ParseError@ to
-- it's string representation before returning it (wrapping it with a @Right@).
-- Therefore, no matter what is the input, the right-hand side of expression in 
-- the where clause is always a @Right@.

  -- parseIntExceptPure "6251"
    -- "The result is: 6251." 
  -- parseIntExceptPure "qwerty"
    -- "Index at 0: Invalid character q!"

-- With @parseIntExceptPure@, we can rewrite @parseIntIOExcept@ in the following
-- way without using explicit case expression.
-- 
parseIntIOExcept' :: IO ()
parseIntIOExcept' = do
  putStrLn "Please type in a positive integer:"
  raw <- getLine
  putStrLn $ parseIntExceptPure raw
  putStrLn "Thank you!"

-- One can check that this function is identical to the old @parseIntIOExcept@.

-- To sum up this section, we first introduced an alternative way of 
-- representing errors, namely wrap the error in a normal type (@Maybe@, @Either
-- or @Except@). In @Maybe@, we cannot include the details of the error; in
-- @Either@, we can record the error specifically, but we usually need a case
-- expression or guard to deal with the normal case (@Right) and the error case
-- (@Left) separately; in @Except@, we have utility functions such as @catchE@
-- to make the flow more natural and similar to imperative try-catch blocks.

-- However, notice that one cannot mix @catchE@ within an @IO@ do block. This is
-- because @Except a@ and @IO@ are different monads. In this section we have 
-- two solutions, namely 1) using a case expression or 2) using a pure 
-- auxilliary function.

-- In the next section, we will talk about the full extent of the @Except@ type,
-- namely @ExceptT@, which is a monad transformer tha allows us to use @catchE@
-- in different monadic contexts.


--------------------------------------------------------------------------------
--  4. ExceptT & Final Example
--------------------------------------------------------------------------------

-- This section requires basic understanding of monad transformers. Maybe it is
-- possible to "copy the format" without understanding it, but I would argue
-- this would be difficult.

parseIntExceptT :: Monad m => String -> ExceptT ParseError m Integer
parseIntExceptT s
  = go s 0 0
  where
    go [] v _       = return v
    go (c : cs) v i = do
      n <- charToInt c i
      go cs (v * 10 + n) (i + 1)
    charToInt ch i
      | isDigit ch = return $ toInteger $ digitToInt ch
      | otherwise  = throwE defaultPE {location = i, char = ch}

-- Note that the body of this function is exactly the same as @parseIntExceptT@,
-- but the signature is more general. it takes the @String@ to be parsed, and
-- returns a monad transformer containing both the @Either@ result and the 
-- current monadic context.

-- The function @runExceptT@ extracts the @Either@ wrapped within under the 
-- context of the monad.
--
parseIntIOExceptT :: IO ()
parseIntIOExceptT
  = void $ runExceptT $ do
      do lift $ putStrLn "Please type in a positive integer:"
         raw <- lift getLine
         n   <- parseIntExceptT raw
         lift $ putStrLn $ "The result is: " ++ show n ++ "."
         `catchE`
         (lift . print)
      lift $ putStrLn "Thank you!"

-- It works exactly the same as @parseIntIOExcept@ and @parseIntIOExcept'@, but
-- with neither case expressions nor helper functions.

-- Finally, here is an exmaple of a parser that turns a @String@ of signed
-- binary numebers into its decimal representation.
-- 
parseBinary :: Monad m => String -> ExceptT ParseError m Integer
parseBinary s
  | null s           = throwE defaultPE {reason = "Cannot parse empty string!"}
  | isDigit (head s) = go s 0 0
  | null $ tail s    = throwE defaultPE 
                       { char   = head s
                       , reason = "Cannot parse singleton non-digit {c}!"
                       }
  | otherwise        = liftM2 (*) sign $ go (tail s) 0 1
  where
    sign = case head s of
      '+' -> return 1
      '-' -> return $ -1
      ch  -> throwE defaultPE {location = 0, char = ch}
    go [] v _       = return v
    go (c : cs) v i = do
      n <- charToInt c i
      go cs (v * 2 + n) (i + 1)
    charToInt ch i
      | ch == '0' || ch == '1' = return $ toInteger $ digitToInt ch
      | otherwise              = throwE defaultPE {location = i, char = ch}

-- Keep prompting for input until its parseable.
-- 
parseBinaryIO :: IO ()
parseBinaryIO 
  = void $ runExceptT parseBinaryT
  where
    parseBinaryT = do 
      lift $ putStrLn "Please type in an integer:"
      raw <- lift getLine
      n   <- parseBinary raw
      lift $ putStrLn $ "The result is: " ++ show n ++ ".\nThank you!"
      `catchE` \e -> do
      lift $ print e
      parseBinaryT
