module Main where

import Control.Exception (throw)
import Data.Char
import Data.List
import System.IO (isEOF)

-- read input infix expression by user
-- split it into tokens, notice that maybe there are not spaces between tokens
-- then convert it to postfix expression
-- and evaluate it and print the result
main :: IO ()
main = do
  end <- isEOF
  if end
    then return ()
    else do
      putStrLn "Enter infix expression:"
      infixExp <- getLine
      let tokens = tokenize infixExp
      putStrLn $ "tokens: " ++ show tokens
      let postfixExp = infixToPostfix tokens
      putStrLn $ "RPN tokens: " ++ show postfixExp
      let result = evaluate postfixExp
      print result
      main

-- tokenize infix expression into tokens
-- Example: "23+3.2*(2+1)" -> ["23", "+", "3.2", "*", "(", "2", "+", "1", ")"]
tokenize :: String -> [String]
tokenize [] = []
tokenize (x : xs) = case x of
  '(' -> "(" : tokenize xs
  ')' -> ")" : tokenize xs
  '+' -> "+" : tokenize xs
  '-' -> "-" : tokenize xs
  '*' -> "*" : tokenize xs
  '/' -> "/" : tokenize xs
  ' ' -> tokenize xs
  _ ->
    let (number, rest) = span isDigitOrDot (x : xs)
     in number : tokenize rest

isDigitOrDot :: Char -> Bool
isDigitOrDot c = isDigit c || c == '.'

-- get priority of operator
getExpPriority :: String -> Int
getExpPriority (x : _)
  | x == '+' || x == '-' = 1
  | x == '*' || x == '/' = 2
  | x == '(' = 0
  | otherwise = throw $ userError ("invalid operator: " ++ [x])

-- convert infix expression to postfix expression
-- Example: ["23", "+", "3.2", "*", "(", "2", "+", "1", ")"] -> ["23", "3.2", "2", "1", "+", "*", "+"]
infixToPostfix :: [String] -> [String]
infixToPostfix tokens = reverse $ infixToPostfix' tokens [] []

-- tokens -> operators stack -> temp output stack -> infix
infixToPostfix' :: [String] -> [String] -> [String] -> [String]
infixToPostfix' [] [] output = output
-- when tokens is empty, put residual operators in temp output stack
infixToPostfix' [] (x : xs) output = infixToPostfix' [] xs (x : output)
infixToPostfix' (x : xs) operators output
  | isNumberString x = infixToPostfix' xs operators (x : output)
  | x == "(" = infixToPostfix' xs (x : operators) output
  | x == ")" =
      let (tempOutput, residualOperators) = span (/= "(") operators
       in infixToPostfix' xs (tail residualOperators) (tempOutput ++ output)
  | otherwise =
      let (tempOutput, residualOperators) = span ((>= getExpPriority x) . getExpPriority) operators
       in infixToPostfix' xs (x : residualOperators) (tempOutput ++ output)

isNumberString :: String -> Bool
isNumberString = all isDigitOrDot

-- evaluate postfix expression
-- Example: ["23", "3.2", "2", "1", "+", "*", "+"] -> 29.2
evaluate :: [String] -> Double
evaluate tokens = head $ foldl foldingFunction [] tokens
  where
    foldingFunction (x : y : ys) "*" = (y * x) : ys
    foldingFunction (x : y : ys) "/" = (y / x) : ys
    foldingFunction (x : y : ys) "+" = (y + x) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction xs numberString = read numberString : xs