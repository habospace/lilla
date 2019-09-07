{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where

import Data.List hiding (insert, tail)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Error
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Data

type LillaEnvironment = Map.Map String LillaVal
type LillaProgram = [LillaVal]
type ThrowsLillaError = Either LillaError
type LillaProgramExecution = StateT LillaEnvironment ThrowsLillaError LillaVal

showLillaEnvironment :: LillaEnvironment -> String
showLillaEnvironment le = join $ formattedEnv where
    formattedEnv = zipWith (\a b -> a ++ b) (format <$> Map.toList le) (repeat "\n") 
    format = (\(var, val) -> (show var) ++ ": " ++ (show val))

data ExecutionContext = 
      Global
    | Function
    deriving (Eq, Show)

run :: LillaProgram -> ThrowsLillaError (LillaVal, LillaEnvironment)
-- runs a LillaProgram with empty environment initialised 
run lp = execute Global lp (return (Null, Map.empty))

execute :: ExecutionContext -> LillaProgram -> ThrowsLillaError (LillaVal, LillaEnvironment) -> 
           ThrowsLillaError (LillaVal, LillaEnvironment)
-- if function gets empty lilla program return input
execute _ [] inp = inp
-- if next expression to evaluate in a Functional context is preceded by a return statement
-- then return expression and ignore all the following expressions
execute Function ((LillaList [AtomicLilla "return", expr]):_) inp = evaluate Function expr inp 
-- if return statement in Functional context is not directly followed by an expression then return Null
execute Function ((LillaList [AtomicLilla "return"]): expr) inp = evaluate Function Null inp 
-- if last expression in a Functional context is not preceded by return statement then return Null
execute Function [] inp = evaluate Function Null inp
-- base case: evaluate program expression by expression recursively
execute context (expr:exprs) inp = execute context exprs (evaluate context expr inp)

evaluate :: ExecutionContext -> LillaVal -> ThrowsLillaError (LillaVal, LillaEnvironment) -> 
            ThrowsLillaError (LillaVal, LillaEnvironment)
-- intermediary function to transfer environments between evaluation ticks
evaluate _ exp (Left err) = throwError err
evaluate context exp (Right (_, env)) = case runStateT ((return exp) >>= (eval context)) env of
    Left err        -> throwError err
    Right result    -> Right result

eval :: ExecutionContext -> LillaVal -> LillaProgramExecution
-- evaluates a Lilla Expression
eval _ Null = return Null
eval _ val@(IntegerLilla _) = return val
eval _ val@(StringLilla _) = return val
eval _ val@(BooleanLilla _) = return val
eval _ val@(AtomicLilla var) = do
    env <- get
    case Map.lookup var env of
        Nothing  -> throwError $ RuntimeLillaError $ "NameError: " ++ (show var) ++ " is not defined."
        Just x   -> return x
eval context (LillaList [AtomicLilla var, AtomicLilla "=", val]) = do
    x <- (eval context) val
    modify (Map.insert var x)
    return val
eval context (LillaList [AtomicLilla "return", expr]) = case context of
    Function  -> (eval context) expr
    Global    -> throwError $ RuntimeLillaError "Cannot return in Global context." 

eval context (LillaList [AtomicLilla "if", pred, LillaList conseqs, 
              AtomicLilla "else", LillaList alts]) = do
        result <- (eval context) pred
        env <- get
        case result of 
            BooleanLilla x -> case execute context (if x then conseqs else alts) (return (Null, env)) of
                Left err  -> throwError $ err
                Right (val, env) -> do
                    put env
                    return val
            x@_              -> throwError $ RuntimeLillaError $ "TypeError: " ++ (show x) ++ " expecting Bool."

eval context (LillaList [AtomicLilla "primitive", AtomicLilla func, args@(LillaList _)]) = do
    args' <- (eval context) args
    case evaluatePrimitiveFunc func args' of
        Left err  -> throwError err
        Right val -> return val

eval context (LillaList [AtomicLilla "defined", AtomicLilla func, args@(LillaList _)]) = do
    env   <- get
    args' <- (eval context) args
    case evaluateUserDefinedFunc func env args' of
        Left err  -> throwError $ err
        Right val -> return val

eval context func@(LillaFunc name _ _) = do 
    modify (Map.insert name func)
    return func

eval context (LillaList xs) = do
    env <- get
    case mapM (\x -> runStateT (return x >>= (eval context)) env) xs of
        Left err -> throwError err
        Right ls -> return $ LillaList $ fst <$> ls

eval context val = throwError $ RuntimeLillaError $ "Bad special form (1). Context:" ++ show context ++ 
                                                    " Val: " ++  show val

evaluateUserDefinedFunc ::  String -> LillaEnvironment -> LillaVal -> ThrowsLillaError LillaVal
evaluateUserDefinedFunc func env (LillaList args) = case Map.lookup func env of
    Nothing -> throwError $ RuntimeLillaError $ "NameError: " ++ func ++ " is not defined."
    Just func'@(LillaFunc _ args' body) -> case (length args) == (length args') of
        True -> case execute Function body (return (Null,  Map.union (Map.fromList $ zip args' args) env)) of
            Right (val, _) -> return val
            Left err       -> throwError err
        False -> throwError $ RuntimeLillaError $ "TypeError: " ++ func ++ " is expecting " ++ (show $ length args') ++ " arguments." 
    Just _                            -> throwError $ RuntimeLillaError $ "TypeError: "  ++ func ++ " is not callable."
    Nothing                           -> throwError $ RuntimeLillaError $ "NameError: " ++ func ++ " is not defined."
evaluateUserDefinedFunc _ _ _ = throwError $ RuntimeLillaError "Bad special form (2)." 

evaluatePrimitiveFunc :: String -> LillaVal -> ThrowsLillaError LillaVal
evaluatePrimitiveFunc func (LillaList args) = case lookup func primitives of
    Nothing    -> throwError $ RuntimeLillaError $ "NameError: " ++ func ++ " is not defined."
    Just func' -> case func' args of
        Left  err -> throwError err
        Right val -> return val
evaluatePrimitiveFunc _ _ = throwError $ RuntimeLillaError "Bad special form (3)." 

primitives :: [(String, [LillaVal] -> ThrowsLillaError LillaVal)]
primitives = [
        ("plus", numericBinop (+)), 
        ("minus", numericBinop (-)), 
        ("mul", numericBinop (*)), 
        ("div", numericBinop div),
        ("mod", numericBinop mod), 
        ("quotient", numericBinop quot), 
        ("remainder", numericBinop rem),
        ("eqv", numBoolBinop (==)),
        ("lt", numBoolBinop (<)),
        ("gt", numBoolBinop (>)),
        ("ne", numBoolBinop (/=)),
        ("gte", numBoolBinop (>=)),
        ("lte", numBoolBinop (<=)),
        ("_eqv", strBoolBinop (==)),
        ("_lt", strBoolBinop (<)),
        ("_gt", strBoolBinop (>)),
        ("_ne", strBoolBinop (/=)),
        ("_gte", strBoolBinop (>=)),
        ("_lte", strBoolBinop (<=)),
        ("and", boolBoolBinop (&&)),
        ("or", boolBoolBinop (||)),
        ("head", head'),
        ("tail", tail'),
        ("cons", cons),
        ("concat", conc),
        ("replicate", repl),
        ("length", length'),
        ("toString", toString),
        ("toNumber", toNumber),
        ("take", take'),
        ("generateList", generateList),
        ("sum", sum'),
        ("max", max'),
        ("min", min'),
        ("split", split),
        ("not", not')
    ]

not' :: [LillaVal] -> ThrowsLillaError LillaVal
not' [BooleanLilla x] = return . BooleanLilla . not $ x
not' [_] = throwError . RuntimeLillaError $ "ValueError: function `not` takes one Bool argument." 
not' _ = throwError . RuntimeLillaError $ "ValueError: function `not` takes exactly one Bool argument." 

split :: [LillaVal] -> ThrowsLillaError LillaVal
split [StringLilla val, StringLilla splittingVal] = if length splittingVal > 1
    then throwError $ RuntimeLillaError "ValueError: split takes splitting string of length 1 character." 
    else return . LillaList $ (\x -> StringLilla x) <$> (split' val splittingVal) where
        split' [] _ = []
        split' word@(x:xs) s = if s == "" then (\c -> [c]) <$>  word else if [x] == s 
            then []
            else [wordUntilSplit] ++ (split' (snd $ splitAt (length wordUntilSplit) word) s) where
                wordUntilSplit = takeWhile (\x' -> x' /= (s !! 0)) word
split _ = throwError . RuntimeLillaError $ "TypeError: split takes exectly two String arguments."

min' :: [LillaVal] -> ThrowsLillaError LillaVal
min' [LillaList xs] = if boolAccumulator xs isNumeric
    then return . IntegerLilla $ minimum $ (\(IntegerLilla x) -> x) <$> xs
    else if boolAccumulator xs isBoolean
        then return . BooleanLilla $ minimum $ (\(BooleanLilla x) -> x) <$> xs
        else if boolAccumulator xs isString 
            then return . StringLilla $ minimum $ (\(StringLilla x) -> x) <$> xs
            else if boolAccumulator xs isCharacter
                then return . CharacterLilla $ minimum $ (\(CharacterLilla x) -> x) <$> xs
                else throwError . RuntimeLillaError $ "ValueError: no msg..."
min' _ = throwError . RuntimeLillaError $ "TypeError: min takes exactly one list argument"

max' :: [LillaVal] -> ThrowsLillaError LillaVal
max' [LillaList xs] = if boolAccumulator xs isNumeric
    then return . IntegerLilla $ maximum $ (\(IntegerLilla x) -> x) <$> xs
    else if boolAccumulator xs isBoolean
        then return . BooleanLilla $ maximum $ (\(BooleanLilla x) -> x) <$> xs
        else if boolAccumulator xs isString 
            then return . StringLilla $ maximum $ (\(StringLilla x) -> x) <$> xs
            else if boolAccumulator xs isCharacter
                then return . CharacterLilla $ maximum $ (\(CharacterLilla x) -> x) <$> xs
                else throwError . RuntimeLillaError $ "ValueError: no msg..."
max' _ = throwError . RuntimeLillaError $ "TypeError: min takes exactly one list argument"
    
sum' :: [LillaVal] -> ThrowsLillaError LillaVal
sum' [LillaList xs] = if allNumeric
    then return . IntegerLilla . sum $ (\(IntegerLilla x) -> x) <$> xs
    else throwError $ RuntimeLillaError "TypeError: expecting a list of Numbers."  where
        allNumeric = foldr (\x acc -> (isNumeric x) && acc) True xs
        isNumeric (IntegerLilla _) = True
        isNumeric _                = False 
sum' [_] = throwError $ RuntimeLillaError "TypeError: expecting a list of Numbers."
sum' _ = throwError $ RuntimeLillaError "TypeError: sum takes exactly one list argument." 

generateList :: [LillaVal] -> ThrowsLillaError LillaVal
generateList [IntegerLilla min, IntegerLilla max] = return . LillaList $ (\x -> IntegerLilla x) <$> [min .. max]
generateList [IntegerLilla min, _] = throwError $ RuntimeLillaError "TypeError: expecting a Number as 2nd argument."
generateList [_, IntegerLilla max] = throwError $ RuntimeLillaError "TypeError: expecting a Number as 1st argument."
generateList [_, _] = throwError $ RuntimeLillaError "TypeError: expecting Numbers as arguments"
generateList _ = throwError $ RuntimeLillaError "TypeError: `generateList` takes exactly 2 arguments."

take' :: [LillaVal] -> ThrowsLillaError LillaVal
take' [IntegerLilla n, LillaList xs] = return . LillaList $ take (fromIntegral n) xs
take' [IntegerLilla n, _] = throwError $ RuntimeLillaError "TypeError: expecting a List as 2nd argument."
take' [_, LillaList xs]   = throwError $ RuntimeLillaError "TypeError: expecting a Number as 1st argument."
take' [_, _] = throwError $ RuntimeLillaError "TypeError: expecting Number and List arguments."
take' _ = throwError $ RuntimeLillaError "TypeError: `take` takes exactly 2 arguments."

toNumber :: [LillaVal] -> ThrowsLillaError LillaVal
toNumber [StringLilla val] = if numericString
    then return . IntegerLilla . (\x -> read x :: Integer) $ val
    else throwError $ RuntimeLillaError "ValueError: toNumber takes only numeric strings" where 
        numericString = foldr (\c acc -> (c `elem` ['0' .. '9']) && acc) True val
toNumber [_] = throwError $ RuntimeLillaError "TypeError: toNumber takes argument of type string."
toNumber (x:xs) = throwError $ RuntimeLillaError "TypeError: toString takes exactly 1 argument."

toString :: [LillaVal] -> ThrowsLillaError LillaVal
toString [x] = return $ StringLilla . show $ x
toString (x:xs) = throwError $ RuntimeLillaError "TypeError: toString takes exactly 1 argument."

length' :: [LillaVal] -> ThrowsLillaError LillaVal
length' [LillaList xs] = return $ IntegerLilla . fromIntegral . length $ xs
length' val = throwError $ RuntimeLillaError $ "TypeError: length takes lists. Got " ++ (show val) ++ " instead."

head' :: [LillaVal] -> ThrowsLillaError LillaVal
head' [LillaList (x:_)] = return x
head' [LillaList _] = throwError $ RuntimeLillaError "Error: no head of empty list."
head' (x:xs) = throwError $ RuntimeLillaError "TypeError: head takes exactly 1 argument."
head' _ = throwError $ RuntimeLillaError "Bad special form (4)."

tail' :: [LillaVal] -> ThrowsLillaError LillaVal
tail' [LillaList (_:xs)] = return $ LillaList xs
tail' [LillaList _] = throwError $ RuntimeLillaError "Error: no tail of empty list."
tail' (x:xs) = throwError $ RuntimeLillaError "TypeError: head takes exactly 1 argument."
tail' _ = throwError $ RuntimeLillaError "Bad special form (5)."

cons :: [LillaVal] -> ThrowsLillaError LillaVal
cons [x, LillaList []] = return $ LillaList [x]
cons [x, LillaList xs] = return $ LillaList (x:xs)
cons [_, _] = throwError $ RuntimeLillaError "TypeError: expecting List as 2nd argument."
cons _ = throwError $ RuntimeLillaError "TypeError: cons takes exactly 2 arguments."

conc :: [LillaVal] -> ThrowsLillaError LillaVal
conc [LillaList xs, LillaList xs'] = return $ LillaList $ xs ++ xs'
conc [_, _] = throwError $ RuntimeLillaError "TypeError: concat takes exactly 2 arguments of type List."
conc _ =throwError $ RuntimeLillaError "TypeError: concat takes exactly 2 arguments."

repl :: [LillaVal] -> ThrowsLillaError LillaVal
repl [IntegerLilla n, LillaList xs] = return $ LillaList $ concat $ replicate (fromIntegral n) xs
repl [_, LillaList _] = throwError $ RuntimeLillaError "TypeError: expecting Integer as 1st argument."
repl [IntegerLilla _, _] = throwError $ RuntimeLillaError "TypeError: expecting List as 2nd argument."
repl [_, _] = throwError $ RuntimeLillaError "TypeError: expecting arguments of types Integer and List."
repl _ = throwError $ RuntimeLillaError "TypeError: repl takes exactly 2 arguments."

boolBinop :: (LillaVal -> ThrowsLillaError a) -> (a -> a -> Bool) -> [LillaVal] -> ThrowsLillaError LillaVal
boolBinop unpacker op args 
    | length args /= 2 = 
        throwError $ RuntimeLillaError $ "TypeError: function is expecting 2 args, currently has:"  ++ (show $ length args) ++ "."
    | otherwise        = do
        left <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return . BooleanLilla $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LillaVal -> ThrowsLillaError String
unpackStr (StringLilla s) = return s
unpackStr notString = throwError $ RuntimeLillaError "Typerror: expecting String." 

unpackBool :: LillaVal -> ThrowsLillaError Bool
unpackBool (BooleanLilla b) = return b
unpackBool notBool = throwError $ RuntimeLillaError "Typerror: expecting Bool."

numericBinop :: (Integer -> Integer -> Integer) -> [LillaVal] -> ThrowsLillaError LillaVal
numericBinop op [_] = throwError $ RuntimeLillaError "TypeError: expecting 2 arguments, got 1 instead."
numericBinop op params = mapM unpackNum params >>= return . IntegerLilla . foldl1 op

unpackNum :: LillaVal -> ThrowsLillaError Integer
unpackNum (IntegerLilla n) = return n
unpackNum _ = throwError $ RuntimeLillaError "Typerror: expecting Integer."

isNumeric :: LillaVal -> Bool
isNumeric (IntegerLilla _) = True
isNumeric _ = False

isBoolean :: LillaVal -> Bool
isBoolean (BooleanLilla _) = True
isBoolean _ = False

isString :: LillaVal -> Bool
isString (StringLilla _) = True
isString _ = False

isCharacter :: LillaVal -> Bool
isCharacter (CharacterLilla _) = True
isCharacter _ = False

boolAccumulator :: [LillaVal] -> (LillaVal -> Bool) -> Bool
boolAccumulator xs' f = foldr (\x' acc' -> x' && acc') True $ f <$> xs'

checkIfHomogenous :: [LillaVal] -> Bool
checkIfHomogenous xs = foldr (\x acc -> x || acc) False typeHomogenities where
    typeHomogenities = [
        boolAccumulator xs isNumeric, 
        boolAccumulator xs isBoolean, 
        boolAccumulator xs isString, 
        boolAccumulator xs isCharacter]
