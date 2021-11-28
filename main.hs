{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative (Alternative (..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace, ord)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)

data JValue
  = JNull
  | JBool Bool
  | JString String
  | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq, Generic)

instance Show JValue where
  show value = case value of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JString s -> showJSONString s
    JNumber s [] 0 -> show s
    JNumber s f 0 -> show s ++ "." ++ concatMap show f
    JNumber s [] e -> show s ++ "e" ++ show e
    JNumber s f e -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
    JArray a -> "[" ++ intercalate ", " (map show a) ++ "]"
    JObject o -> "{" ++ intercalate ", " (map showKV o) ++ "}"
    where
      showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/' -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
  _ -> [c]
  where
    showJSONNonASCIIChar c =
      let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a

newtype Parser i o = Parser {runParser :: i -> Maybe (i, o)}

satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
  (x : xs) | predicate x -> Just (xs, x)
  _ -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

instance Functor (Parser i) where
  fmap f parser = Parser $ fmap (fmap f) . runParser parser

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

instance Applicative (Parser i) where
  pure x = Parser $ pure . (,x)
  pf <*> po = Parser $ \input -> case runParser pf input of
    Nothing -> Nothing
    Just (rest, f) -> fmap f <$> runParser po rest

string :: String -> Parser String String
string "" = pure ""
string (c : cs) = (:) <$> char c <*> string cs

jNull :: Parser String JValue
jNull = string "null" $> JNull

instance Alternative (Parser i) where
  empty = Parser $ const empty
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

instance Monad (Parser i) where
  p >>= f = Parser $ \input -> case runParser p input of
    Nothing -> Nothing
    Just (rest, o) -> runParser (f o) rest

jBool :: Parser String JValue
jBool =
  string "true" $> JBool True
    <|> string "false" $> JBool False

jsonChar :: Parser String Char
jsonChar =
  string "\\\"" $> '"'
    <|> string "\\\\" $> '\\'
    <|> string "\\/" $> '/'
    <|> string "\\b" $> '\b'
    <|> string "\\f" $> '\f'
    <|> string "\\n" $> '\n'
    <|> string "\\r" $> '\r'
    <|> string "\\t" $> '\t'
    <|> unicodeChar
    <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
  where
    unicodeChar =
      chr . fromIntegral . digitsToNumber 16 0
        <$> (string "\\u" *> replicateM 4 hexDigit)

    hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base =
  foldl (\num d -> num * fromIntegral base + fromIntegral d)

highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound = 0xDC00
lowSurrogateUpperBound = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
  ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a =
  ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b =
  chr $
    ((ord a - highSurrogateLowerBound) `shiftL` 10)
      + (ord b - lowSurrogateLowerBound)
      + 0x10000

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString') -- 1
  where
    jString' = do
      optFirst <- optional jsonChar -- 2
      case optFirst of
        Nothing -> "" <$ char '"' -- 3
        Just first
          | not (isSurrogate first) -> -- 4
            (first :) <$> jString' -- 5
        Just first -> do
          -- 6
          second <- jsonChar -- 7
          if isHighSurrogate first && isLowSurrogate second -- 8
            then (combineSurrogates first second :) <$> jString' -- 9
            else empty -- 10