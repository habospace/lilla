module Main where

import Evaluator
import Parser
import Data
import Text.ParserCombinators.Parsec 

main :: IO ()
main = undefined

getFileContent :: String -> IO String
getFileContent path = do
    content <- getFileContent path
    return content

printFileContent :: String -> IO ()
printFileContent path = do
    content <- readFile path
    putStr content

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

trapError :: ThrowsLillaError (LillaVal, LillaEnvironment) -> String
trapError (Left err) = show err
trapError (Right (_, env)) = showLillaEnvironment env 

runLillaProgram :: String -> IO ()
runLillaProgram path = do
    standards <- readLillaProgram "standards/standards.li"
    lillaProgram <- readLillaProgram path
    output <- return $ trapError $ (convertError (chain standards lillaProgram)) >>= run
    putStr output where
        convertError (Left err) = Left . ParseLillaError . show $ err
        convertError (Right val) = return val
        chain (Right p1) (Right p2) = Right $ p1 ++ p2
        chain (Left err) _ = Left err
        chain _ (Left err) = Left err
