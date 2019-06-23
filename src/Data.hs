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
showLillaVal (AtomicLilla x)    = "AtomicLilla " ++ x
showLillaVal (LillaList xs)     = "LillaList [" ++  (intercalate ", " (showLillaVal <$> xs)) ++ "]"
showLillaVal (NumericLilla x)   = "NumericLilla " ++ (show x)
showLillaVal (StringLilla x)    = "StringLilla " ++ x
showLillaVal (CharacterLilla x) = "CharacterLilla " ++ (show x)
showLillaVal (BooleanLilla x)   = "BooleanLilla " ++ (show x)
showLillaVal (LillaFunc params body) = "No string form of functions yet... sorry" 

instance Show LillaVal where
  show = showLillaVal

-- TODO: simplify this LillaError / remove unnecessary data constructors + change names
data LillaError =
    ParseLillaError
  | CustomLillaError
  | NumArgsLillaError
  | Default String
  deriving (Eq, Show)

instance Error LillaError where
  noMsg = Default "An error has occurred."
  strMsg = Default