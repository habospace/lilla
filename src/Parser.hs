--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ExistentialQuantification #-}

module Parser where 

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import Control.Monad.Trans.Except
import Data
import Evaluator
import Control.Monad


parseFunc :: Parser LillaVal
parseFunc = undefined

parseExpr :: Int -> Parser LillaVal
parseExpr indentation = parseExpression 
    <|> parsePrimitive
    <|> parseExpr indentation where
        parseExpression = do
            x <- (try $ parseAssignment indentation) 
                <|> (try $ parseFuncCall indentation) 
                <|> parseAtom
            return x
        parsePrimitive =  parseNumber
            <|> parseString
            <|> parseBool

parseNumber :: Parser LillaVal
parseNumber = liftM (NumericLilla . read) $ many1 digit 

parseAtom :: Parser LillaVal
parseAtom = do
    first <- letter
    rest <- many (letter <|> digit)
    let atom = [first] ++ rest
    return $ AtomicLilla atom

parseBool :: Parser LillaVal
parseBool = do
    boolVal <- (makeStringParser "True") <|> (makeStringParser "False")
    return $ BooleanLilla . read $ boolVal

parseString :: Parser LillaVal
parseString = do
    char '"'
    x <- many (escapeCharParser <|> noneOf "\"")
    char '"'
    return $ StringLilla x where
        escapeCharParser = 
            makeEscapeCharParsers [
                ("\\\n", '\n'), 
                ("\\\r", '\r'),
                ("\\\t", '\t'),
                ("\\", '\\')
            ]

parseFuncCall :: Int -> Parser LillaVal
parseFuncCall indentation =  do
    makeStringParser $ replicate indentation ' '
    func <- ignore (char ' ') parseAtom
    ignore (char ' ') (char '(')
    args <- sepBy (ignore (char ' ') (parseExpr 0)) (char ',')
    ignore (char ' ') (char ')')
    return $ if elem (unpack func) (fst <$> primitives)
        then LillaList [AtomicLilla "primitive", func, LillaList args]
        else LillaList [func, LillaList args] where
            unpack = (\(AtomicLilla x) -> x)

parseAssignment :: Int -> Parser LillaVal
parseAssignment indentation =  do
    makeStringParser $ replicate indentation ' '
    var <- parseAtom
    assignment <- ignore (char ' ') (makeStringParser "=")
    expr <- ignore (char ' ') (parseExpr 0)
    return $ LillaList [var, AtomicLilla assignment, expr]
   
ignore :: Parser a -> Parser b -> Parser b
ignore pa pb = do
  many pa
  b <- pb
  many pa
  return b

makeStringParser :: String -> Parser String
makeStringParser [] = return ""
makeStringParser (c:cs) = do
    x <- char c
    xs <- makeStringParser cs
    return $ x:xs
    
makeEscapeCharParser :: String -> Char -> Parser Char
makeEscapeCharParser [] c = return c
makeEscapeCharParser (c:cs) c' = do
    char c
    makeEscapeCharParser cs c' 
    
makeEscapeCharParsers :: [(String, Char)] -> (Parser Char)
makeEscapeCharParsers [] = error "empty"
makeEscapeCharParsers ((str, c):tail) = 
    foldr (\(str', c') acc -> (makeEscapeCharParser str' c') <|> acc) (makeEscapeCharParser str c) tail






