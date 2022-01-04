module Eint where

import Control.Applicative((<|>))
import Data.Char
import System.IO
import Text.ParserCombinators.ReadP

getOp :: Char -> (Int -> Int -> Int)
getOp '^' = (^)
getOp '-' = (-)
getOp '/' = div
getOp '*' = (*)
getOp _   = (+)

spaces :: ReadP String
spaces = many (satisfy (== ' '))

factor :: ReadP Int
factor = read <$> (spaces *> many1 (satisfy isDigit) <* spaces)

termRight :: ReadP (Int -> Int)
termRight = do
  op <- char '*' <|> char '/'
  getOp op <$> term

term :: ReadP Int
term = do
  n1 <- factor
  f <- option (+ 0) termRight
  return (f n1)

exprRight :: ReadP (Int -> Int)
exprRight = do
  op <- char '+' <|> char '-'
  getOp op <$> expr

expr :: ReadP Int
expr = do
  n1 <- term
  f <- option (+ 0) exprRight
  return (f n1)

parseExpr :: String -> Maybe Int
parseExpr input = if null result then Nothing else Just $ fst $ last result
  where
    result = readP_to_S expr input

printResult :: Maybe Int -> IO ()
printResult Nothing  = putStrLn "Enter a valid number"
printResult (Just x) = print x

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- getLine
  if null input
  then main
  else case parseExpr input of
    Nothing  -> return ()
    (Just x) -> do
      print x
      main
