module Main where

import System.IO (isEOF)

main :: IO ()
main = do
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      putStrLn $ "tokens: " ++ show (tokenize line)
      putStrLn $ "RPN tokens: " ++ show (inFixToRPN $ tokenize line)
      print $ solveInfix line
      main

a = head (foldl foldFunc [] ["rpnTokens"])

solveInfix :: String -> Int
solveInfix line = head (foldl foldFunc [] (inFixToRPN $ tokenize line))

foldFunc :: [Int] -> String -> [Int]
foldFunc (x : y : ys) "*" = (x * y) : ys
foldFunc (x : y : ys) "+" = (x + y) : ys
foldFunc (x : y : ys) "-" = (y - x) : ys
foldFunc xs numberString = read numberString : xs

specialToken = "()+-*/"

tokenize :: String -> [String]
tokenize input =
  let result = foldl traverseChar ([], "") input
      buffer = snd result
   in reverse
        ( case buffer of
            [] -> fst result
            buffer -> reverse buffer : fst result
        )

traverseChar :: ([String], String) -> Char -> ([String], String)
traverseChar (tokens, buffer) token
  | token `elem` specialToken = case buffer of
      [] -> ([token] : tokens, "")
      buffer -> ([token] : reverse buffer : tokens, "")
traverseChar old ' ' = old
traverseChar (tokens, buffer) token = (tokens, token : buffer)

getExpPriority :: String -> Int
getExpPriority (x : _)
  | x == '+' || x == '-' = 1
  | x == '*' || x == '/' = 2

inFixToRPN :: [String] -> [String]
inFixToRPN tokens =
  let (numStack, expStack) = foldl traverseToken ([], []) tokens
   in reverse numStack ++ expStack

traverseToken :: ([String], [String]) -> String -> ([String], [String])
traverseToken (numStack, expStack) token
  | length token > 1 = (token : numStack, expStack)
  | let [chartoken] = token in chartoken `notElem` specialToken = (token : numStack, expStack)
traverseToken (numStack, expStack) "(" = (numStack, "(" : expStack)
traverseToken (numStack, expStack) ")" =
  let (pops, rest) = span (/= "(") expStack
   in case rest of
        [] -> (reverse pops ++ numStack, [])
        rest -> (reverse pops ++ numStack, tail rest)
traverseToken (numStack, []) token = (numStack, [token])
traverseToken (numStack, expStack) token
  | getExpPriority token > getExpPriority (last expStack) = (numStack, token : expStack)
  | otherwise =
      let (pops, t : rest) = span (\elem -> elem /= "(" && getExpPriority token > getExpPriority elem) expStack
       in case t of
            "(" -> (reverse pops ++ numStack, token : rest)
            t -> (reverse pops ++ [t] ++ numStack, token : rest)