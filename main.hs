module Main where

import Data.Char
import Data.List
import Control.Applicative
import System.Environment

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- Общий тип парсер, который возвращает объект JsonValue
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

-- Вспомогательные методы и функции парсера
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
      Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP template = sequenceA $ map charP template

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

-- Парсинг null
jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

--Парсинг bool значений
jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
   where f "true"  = JsonBool True
         f "false" = JsonBool False
         f _       = undefined

--Парсинг number значений
jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where f ds = JsonNumber $ read ds

-- Проверка на начало строки
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

-- Парсинг строки
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

-- Проверка на пробельный символ
ws :: Parser String
ws = spanP isSpace

-- Разделение элементов по символу
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

-- Парсинг массива
jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *>
                           elements
                           <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

-- Парсинг json объекта
jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where
    pair =
      (\key _ value -> (key, value)) <$> stringLiteral <*>
      (ws *> charP ':' <* ws) <*>
      jsonValue

-- общий парсер
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

-- парсинг файла
parseFile :: FilePath -> IO (Maybe JsonValue)
parseFile fileName = do
  input <- readFile fileName
  return (snd <$> runParser jsonValue input)

translateElementInArray :: JsonValue -> String -> String
translateElementInArray value startString =
  case value of
    JsonObject ((fstKey, fstValue) : []) -> fstKey ++ ": " ++ (innerTranslateYaml fstValue startString)
    JsonObject ((fstKey, fstValue) : xs) -> fstKey ++ ": " ++ (innerTranslateYaml fstValue startString) ++ "\n" ++ 
      ((intercalate "\n" $ map (\(key, value) -> startString ++ "  " ++ key ++ ": " ++ (innerTranslateYaml value (startString ++ "    "))) xs))
    _ -> translateToYaml value startString

innerTranslateYaml :: JsonValue -> String -> String
innerTranslateYaml value startString = 
  case value of
    JsonArray values -> "\n" ++ (intercalate "\n" $ map (\x -> startString ++ "- " ++ (translateElementInArray x startString)) values)
    JsonObject values -> "\n" ++ (intercalate "\n" $ map (\(key, value) -> startString ++ key ++ ": " ++ (innerTranslateYaml value (startString ++ "  "))) values)
    _ -> translateToYaml value startString

-- перевод в yaml формат
translateToYaml :: JsonValue -> String ->String
translateToYaml value startString = 
  case value of
    JsonNull -> "null"
    JsonNumber value -> show value
    JsonBool value -> show value
    JsonString value -> "\"" ++ value ++ "\""
    JsonArray values -> intercalate "\n" $ map (\x -> startString ++ "- " ++ (innerTranslateYaml x (startString ++ "  "))) values
    JsonObject values -> intercalate "\n" $ map (\(key, value) -> startString ++ key ++ ": " ++ (innerTranslateYaml value (startString ++ "  "))) values


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      parseResult <- (parseFile inputFile)
      case parseResult of
        Just actualJsonAst -> do
          writeFile outputFile $ translateToYaml actualJsonAst ""
        Nothing -> do
          putStrLn "ERROR"
    
    _ -> putStrLn "Invalid arguments"