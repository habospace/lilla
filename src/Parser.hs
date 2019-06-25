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


parseIfElseExpr :: Int -> Parser LillaVal
parseIfElseExpr indentation = do
    if' <- ignore (char ' ') (makeStringParser "if")
    ignore (char ' ') (char ':')
    -- replicate (indentation + 4) " "
    return Null

parseFunc :: Int -> Parser LillaVal
parseFunc indentation = do
    makeStringParser "function"
    name <- ignore (char ' ') word
    ignore (char ' ') (char '(')
    args <- sepBy (ignore (char ' ') word ) (char ',')
    ignore (char ' ') (char ')')
    ignore (char ' ') (char '\n')
    return Null

word :: Parser String
word = do
    first <- letter
    rest <- many (letter <|> digit)
    return $ [first] ++ rest


parseExpr :: Parser LillaVal
parseExpr = parseExpression 
    <|> parsePrimitive
    <|> parseExpr where
        parseExpression = do
            x <- (try $ parseAssignment ) 
                <|> (try $ parseFuncCall ) 
                <|> parseAtom
            return x
        parsePrimitive =  parseNumber
            <|> parseString
            <|> parseBool

parseReturnStatement :: Parser LillaVal
parseReturnStatement = do
    makeStringParser "return"
    expr <- ignore (char ' ') parseExpr
    return $ LillaList [AtomicLilla "return", expr]

parseNumber :: Parser LillaVal
parseNumber = liftM (NumericLilla . read) $ many1 digit 

parseAtom :: Parser LillaVal
parseAtom = do
    atom <- word
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

parseFuncCall :: Parser LillaVal
parseFuncCall =  do
    func <- ignore (char ' ') parseAtom
    ignore (char ' ') (char '(')
    args <- sepBy (ignore (char ' ') parseExpr ) (char ',')
    ignore (char ' ') (char ')')
    return $ if elem (unpack func) (fst <$> primitives)
        then LillaList [AtomicLilla "primitive", func, LillaList args]
        else LillaList [func, LillaList args] where
            unpack = (\(AtomicLilla x) -> x)

parseAssignment :: Parser LillaVal
parseAssignment =  do
    var <- parseAtom
    assignment <- ignore (char ' ') (makeStringParser "=")
    expr <- ignore (char ' ') parseExpr
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
