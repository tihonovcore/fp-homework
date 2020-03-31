module Block6
  ( CBS
  , Parser (..)
  , check
  , commaThenNumberParser
  , element
  , eof
  , listlistParser
  , listParser
  , ok
  , parseClose
  , parseComma
  , parseDigit
  , parseCBS
  , parseNumber
  , parseNumberWS
  , parseOpen
  , parseNumberWithAccum
  , parseSign
  , satisfy
  , skipWs
  , stream
  ) where

import Data.Char
import Control.Applicative (Alternative (..))

-----------------------------
-- Task 1 -------------------
-----------------------------

data Parser e a = Parser { runParser :: [e] -> Maybe (a, [e]) }

instance Functor (Parser e) where
  fmap f (Parser parse) = Parser $ \s ->
    fmap (\(l, r) -> ((f l), r)) (parse s)

instance Applicative (Parser e) where
  pure x = Parser $ \s -> Just (x, s)

  (<*>) (Parser parseFunction) (Parser parseArgument) = Parser $ \s ->
    parseFunction s >>= \(f, arguments) -> parseArgument arguments
                    >>= \(a, other) -> Just (f a, other)

instance Alternative (Parser e) where
  empty = Parser $ \_ -> Nothing

  (<|>) (Parser l) (Parser r) = Parser $ \s ->
    (l s) <|> (r s)

instance Monad (Parser e) where
  (>>=) (Parser parse) f = Parser $ \s ->
    parse s >>= \(result, other) -> runParser (f result) other


-----------------------------
-- Task 2 -------------------
-----------------------------

-- |Parser that always success
ok :: Parser e ()
ok = Parser $ \s -> Just ((), s)

-- |Result of parsing is Just, if end of input reached,
-- and otherwise Nothing
eof :: Parser e ()
eof = Parser $ \s -> case s of
                       [] -> Just ((), [])
                       _  -> Nothing

-- |Check that first element in stream is satisfy to predicate
-- and return it, otherwise - Nothing
satisfy :: (e -> Bool) -> Parser e e
satisfy p = Parser $ \s ->
  case s of
    []       -> Nothing
    (x : xs) -> if p x then Just (x, xs) else Nothing

-- |Return first element in stream if it exists
element :: Parser e e
element = Parser $ \s ->
  case s of
    []       -> Nothing
    (x : xs) -> Just (x, xs)

-- |Return first `n` elements in stream if it exists
stream :: Int -> Parser e [e]
stream n | n == 0    = Parser $ \s -> Just ([], s)
         | n >  0    = Parser $ \s -> (runParser element s) >>=
                                \(first, other) -> runParser (stream (n - 1)) other >>=
                                \(elements, other1) -> Just (first : elements, other1)
         | otherwise = Parser $ \_ -> Nothing


-------------------------------
---- Task 3 -------------------
-------------------------------

-- |Interpretation of correct bracket sequences:
-- empty CBS is CBS, concatenation of CBS is CBS and
-- CBS in brackets is CBS 
data CBS = Empty | Pair CBS CBS | InBrackets CBS

instance Show CBS where
  show Empty = ""
  show (Pair l r) = (show l) ++ (show r)
  show (InBrackets cbs) = "(" ++ (show cbs) ++ ")"

instance Eq CBS where
  Empty == Empty = True
  InBrackets l == InBrackets r = (l == r)
  Pair l1 r1 == Pair l2 r2 = (l1 == l2) && (r1 == r2)
  _ == _ = False

-- |Parse open bracket
parseOpen :: Parser Char Char
parseOpen = (satisfy (\c -> c == '('))

-- |Parse close bracket
parseClose :: Parser Char Char
parseClose = (satisfy (\c -> c == ')'))

-- |Parse correct bracket sequence
parseCBS :: Parser Char CBS
parseCBS = Parser $ \s -> runParser parseS s >>= \result@(_, other) -> runParser eof other >>= \_ -> Just result
  where
    parseInBrackets :: Parser Char CBS
    parseInBrackets = Parser $ \s -> runParser parseOpen s >>=
                               \(_, s1) -> runParser parseS s1 >>=
                               \(cbs, s2) -> runParser parseClose s2 >>=
                               \(_, other) -> Just (InBrackets cbs, other)

    parseS :: Parser Char CBS
    parseS = Parser $ \s ->
      case runParser (check (\c -> c == '(')) s of
        Just (_, s0) -> runParser parseInBrackets s0 >>= \(left, s1) -> runParser parseS s1
                                                   >>= \(right, s2) -> Just (Pair left right, s2)
        Nothing -> Just (Empty, s)

-- |Check that first element in stream is satisfy to predicate
-- and return it, otherwise - Nothing. NOTE: `check` doesn't 
-- change stream
check :: (e -> Bool) -> Parser e e
check p = Parser $ \s ->
  case s of
    []      -> Nothing
    (x : _) -> if p x then Just (x, s) else Nothing

-- |Parse digit
parseDigit :: Parser Char Char
parseDigit = satisfy isDigit

-- |Parse sign of number. Result is -1 of number is negative, or 1 
-- if number is positive
parseSign :: Parser Char Int
parseSign = fmap charToSign $ satisfy detectSign <|> check isDigit
  where
    detectSign :: Char -> Bool
    detectSign c = (c == '+' || c == '-')

    charToSign :: Char -> Int
    charToSign c | c == '+'  = 1
                 | c == '-'  = -1
                 | otherwise = 1

-- |Parse number. Accumulator contains parsed prefix of number
parseNumberWithAccum :: Int -> Parser Char Int
parseNumberWithAccum accum = Parser $ \s ->
  case runParser parseDigit s of
    Nothing         -> Just (accum, s)
    Just (digit, other) ->
      let newAccum = accum * 10 + (digitToInt digit) in
        runParser (parseNumberWithAccum newAccum) other

-- |Parser for number
parseNumber :: Parser Char Int
parseNumber = Parser $ \s ->
  (runParser parseSign s) >>= \(sign, s1) -> runParser (check isDigit) s1
                          >>= \(_,    s2) -> runParser (parseNumberWithAccum 0) s2
                          >>= \(number, s3) -> Just (sign * number, s3)


-------------------------------
---- Task 4 -------------------
-------------------------------

-- |Skip spaces from stream
wsParser :: Parser Char ()
wsParser = Parser $ \s ->
  case runParser (satisfy isSpace) s of
    Nothing -> Just ((), s)
    Just (_, xs) -> runParser wsParser xs

-- |Skip spaces from stream.
skipWs :: String -> Maybe String
skipWs s = runParser wsParser s >>= \(_, other) -> Just other

-- |Skip spaces and then parse `,`
parseComma :: Parser Char Char
parseComma = Parser $ \input -> skipWs input >>= \s -> runParser (satisfy (\c -> c == ',')) s

-- |Skip spaces and then parse number
parseNumberWS :: Parser Char Int
parseNumberWS = Parser $ \input -> skipWs input >>= \s -> runParser parseNumber s

-- |Parse `n` pairs of comma and number. Returns
-- list of parsed numbers
commaThenNumberParser :: Int -> Parser Char [Int]
commaThenNumberParser size = Parser $ \s ->
  if size == 0
  then Just ([], s)
  else runParser parseComma s >>= \(_, s1) -> runParser parseNumberWS s1
                              >>= \(n, s2) -> runParser (commaThenNumberParser (size - 1)) s2
                              >>= \(other, s3) -> Just (n : other, s3)

-- |Parse `n` - number of elements in list.
-- Then parse list, and return it. All numbers
-- should be separated by comma
listParser :: Parser Char [Int]
listParser = Parser $ \s ->
  runParser parseNumberWS s >>= \(size, s0) ->
    case compare size 0 of
      LT -> Nothing
      EQ -> Just ([], s0)
      GT -> runParser parseComma s0 >>= \(_, s1) -> runParser parseNumberWS s1
                                   >>= \(first, s2) -> runParser (commaThenNumberParser (size - 1)) s2
                                   >>= \(elements, s3) -> Just (first : elements, s3)

-- |Parse `n` - number of elements in list.
-- Then parse list. If next element is number (besides spaces),
-- repeat action. All these lists returns in lists of lists. All numbers
-- should be separated by comma
listlistParser :: Parser Char [[Int]]
listlistParser = Parser $ \s ->
  runParser listParser s >>= \(first, s0) ->
    case runParser parseComma s0 of
      Nothing -> Just ([first], s0)
      Just (_, s1) -> runParser listlistParser s1 >>= \(other, s2) -> Just (first : other, s2)
