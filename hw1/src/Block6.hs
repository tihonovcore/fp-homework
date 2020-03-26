module Block6
  ( CBC
  , Parser
  , eof
  , ok
  , parseDigit
  , parseNumber
  , parseSign
  , satisfy
  , stream
  ) where

import Data.Char

-----------------------------
-- Task 1 -------------------
-----------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
--
--instance Functor (Parser a) where
--  fmap f =

-----------------------------
-- Task 2 -------------------
-----------------------------

ok :: Parser ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser ()
eof = Parser $ \s -> case s of
                       [] -> Just ((), "")
                       _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \s ->
  case s of
    []       -> Nothing
    (x : xs) -> if pred x then Just (x, xs) else Nothing

element :: Parser Char
element = Parser $ \s ->
  case s of
    []       -> Nothing
    (x : xs) -> Just (x, xs)

stream :: Int -> Parser String
stream n | n == 0    = Parser $ \s -> Just ("", s)
         | n >  0    = Parser $ \s -> (runParser element s) >>= f
         | otherwise = Parser $ \s -> Nothing
                         where
                           f :: (Char, String) -> Maybe (String, String)
                           f (c, s) = fmap (g c) (runParser (stream (n - 1)) s)

                           g :: Char -> (String, String) -> (String, String)
                           g new (parsed, other) = (new : parsed, other)

-----------------------------
-- Task 3 -------------------
-----------------------------

data CBS = Empty | Pair CBS CBS | InBrackets CBS

instance Show CBS where
  show Empty = ""
  show (Pair l r) = (show l) ++ (show r)
  show (InBrackets cbs) = "(" ++ (show cbs) ++ ")"

parseOpen :: Parser Char
parseOpen = (satisfy (\c -> c == '('))

parseClose :: Parser Char
parseClose = (satisfy (\c -> c == ')'))

--correctBracketSequence :: Parser CBS
--correctBracketSequence = Parser $ \s ->

parseDigit :: Parser Char
parseDigit = satisfy isDigit

parseSign :: Parser Char
parseSign = satisfy (\c -> c == '+' || c == '-')

-- NOTE: it parse only sign and first digit
parseNumber :: Parser Int
parseNumber = Parser $ \s ->
  (runParser parseSign s) >>= \(sign, s1) -> (runParser parseDigit s1) >>= (finalize sign)
    where
      finalize :: Char -> (Char, String) -> Maybe (Int, String)
      finalize sign (digit, other) = Just ((signToInt sign) * (digitToInt digit), other)

      signToInt :: Char -> Int
      signToInt c | c == '-'  = -1
                  | c == '+'  =  1
                  | otherwise =  0