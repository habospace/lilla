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
      params :: [String],
      body   :: [LillaVal]
  } deriving (Eq)

showLillaVal :: LillaVal -> String
showLillaVal Null               = "Null"
showLillaVal (AtomicLilla x)    = "AtomicLilla " ++ x
showLillaVal (LillaList xs)     = "LillaList [" ++  (intercalate ", " (showLillaVal <$> xs)) ++ "]"
showLillaVal (NumericLilla x)   = "NumericLilla " ++ (show x)
showLillaVal (StringLilla x)    = "StringLilla " ++ x
showLillaVal (CharacterLilla x) = "CharacterLilla " ++ (show x)
showLillaVal (BooleanLilla x)   = "BooleanLilla " ++ (show x)
showLillaVal (LillaFunc params body) = "\nLillaFunc.\nparams:\n" ++ (show params) ++ 
                                       "\nbody:\n" ++ (show body) ++ "\n"

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