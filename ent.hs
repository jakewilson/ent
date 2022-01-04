module Eint where

import Control.Applicative((<|>))
import Data.Char
import Prelude hiding (exp)
import System.IO (hFlush, stdout)
import Text.ParserCombinators.ReadP

getOp :: Char -> (Int -> Int -> Int)
getOp = flip . getOp'
  where
    getOp' '^' = (^)
    getOp' '-' = (-)
    getOp' '/' = div
    getOp' '*' = (*)
    getOp' _   = (+)

spaces :: ReadP String
spaces = many (satisfy (== ' '))

factor :: ReadP Int
factor = read <$> (spaces *> many1 (satisfy isDigit) <* spaces)

expRight :: ReadP (Int -> Int)
expRight = do
  op <- char '^'
  getOp op <$> exp

exp :: ReadP Int
exp = do
  n <- factor
  f <- option (+ 0) expRight
  return $ f n

termRight :: ReadP (Int -> Int)
termRight = do
  op <- char '*' <|> char '/'
  getOp op <$> term

term :: ReadP Int
term = do
  n1 <- exp
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
