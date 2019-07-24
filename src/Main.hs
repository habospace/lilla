module Main where

import Evaluator
import Parser
import Data
import Text.ParserCombinators.Parsec 


main :: IO ()
main = undefined

printFileContent :: String -> IO ()
printFileContent path = do
    content <- readFile path
    putStr content

readfile :: String -> IO String
readfile path = do
    content <- readFile path
    return content

readLillaProgram :: String -> IO (Either ParseError LillaProgram)
readLillaProgram path = do
    content <- readFile path
    return $ parse (parseExprs 0) "lillaParser" content

printLillaProgram :: String -> IO ()
printLillaProgram path = readLillaProgram path >>= putStr . show 


runParserOnScript :: String -> (String -> (Either ParseError LillaVal)) ->
     IO (Either ParseError LillaVal)
runParserOnScript path f = do
    content <- readFile path
    putStr content
    return $ f content

g = parse (parseIfElseBlock 0) "x"
x = runParserOnScript "lillaprogram.txt" g

h = parse (parseFunc 0) "x"
y = runParserOnScript "testlillaparser.txt" h

i = parse (parseExprs 0) "x"
z = printLillaProgram "lillaprogram.txt"
