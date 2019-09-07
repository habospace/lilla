{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data where

import Control.Monad.Error
import Data.List hiding (insert, tail)

data LillaVal = 
    Null
  | AtomicLilla String
  | LillaList [LillaVal]
  | NumericLilla Integer
  | StringLilla String
  | CharacterLilla Char
  | BooleanLilla Bool
  | LillaFunc {
      name   :: String,
      params :: [String],
      body   :: [LillaVal]
  } deriving (Eq)

showLillaVal :: LillaVal -> String
showLillaVal Null               = "Null"
showLillaVal (AtomicLilla x)    = "Atom " ++ x
showLillaVal (LillaList xs)     = show xs --"LillaList [" ++  (intercalate ", " (showLillaVal <$> xs)) ++ "]"
showLillaVal (NumericLilla x)   = show x --"NumericLilla " ++ (show x)
showLillaVal (StringLilla x)    = "\"" ++ x ++ "\"" --"StringLilla " ++ x
showLillaVal (CharacterLilla x) = show x --"CharacterLilla " ++ (show x)
showLillaVal (BooleanLilla x)   = show x --"BooleanLilla " ++ (show x)
showLillaVal (LillaFunc name params body) = "\nLillaFunc: " ++ name ++ "\nparams:\n" ++ 
                                            (show params) ++ "\nbody:\n" ++ (show body) ++ "\n"

instance Show LillaVal where
  show = showLillaVal

data LillaError =
    DefaultLillaError String
  | ParseLillaError String
  | RuntimeLillaError String
  deriving (Eq, Show)

instance Error LillaError where
  noMsg = DefaultLillaError "An error has occurred."
  strMsg = DefaultLillaError


  