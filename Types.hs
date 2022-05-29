module Types where

import AbsGrammar
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Data.Map as M
import Data.Maybe
import PrintGrammar

type Loc = Integer

type EnvT = M.Map Ident (Bool, Type)

type EnvI = M.Map Ident Loc 

type Store = M.Map Loc Value 

data Flag = Break | Continue | Return Value | Empty

type InterpreterMonad = ReaderT EnvI (StateT Store (ExceptT RuntimeException IO))

data Value = BoolV Bool | IntV Integer | StrV String | NoneV | FunV Type EnvI [Ident] Block | ListV [Value] | TupleV [Value] | LocV Loc
    deriving (Eq, Show)

type Position = BNFC'Position

type TypeCheckerMonad a = ReaderT EnvT (ExceptT TypeCheckerException Identity) a

data RuntimeException = ZeroDimensionException Position | IndexOutOfRange Position Integer | ZeroDevisionException Position | ZeroModException Position  | RangeException Position Integer Integer

data TypeCheckerException = WrongTypeAddException Position Type Type | WrongTypeExceptionListStr Position Type | IndexOutOfDimension Position | DoubleDeclarationException Position Ident Position | ContinueOutsideLoop Position | BreakOutsideLoop Position | ReturnOutsideFunction Position | WrongLengthTupleAssignmentException Position Int Int | NotSimpleTypeException Position Type | CantAssignToConstVariable Position Ident | WrongReturnException Position Type Type | WrongTypeExceptionBool Position Type | WrongTypeExceptionTuple Position Type | WrongTypeExceptionList Position Type | WrongTypeExceptionInt Position Type|  WrongTypeException Position Type Type | NotCallableException Position Ident | WrongNumberOfParametersException Position Ident Int Int | ObjectNotSubscriptableException Position Type Expr | ArrayIndicesNotIntException Position Type Expr | NoSuchVariableOrFunctionException Position Ident

getTypeCheckerExceptionLine:: TypeCheckerException -> Int
getTypeCheckerExceptionLine e = l
    where (BNFC'Position l k) = getTypeCheckerExceptionPosition e

getRuntimeExceptionLine:: RuntimeException -> Int
getRuntimeExceptionLine e = l
    where (BNFC'Position l k) = getRuntimeExceptionPosition e

getTypeCheckerExceptionColumn:: TypeCheckerException -> Int
getTypeCheckerExceptionColumn e = k 
    where (BNFC'Position l k) = getTypeCheckerExceptionPosition e

getRuntimeExceptionColumn:: RuntimeException -> Int
getRuntimeExceptionColumn e = k 
    where (BNFC'Position l k) = getRuntimeExceptionPosition e

getRuntimeExceptionPosition:: RuntimeException -> Position
getRuntimeExceptionPosition (IndexOutOfRange pos _) = pos
getRuntimeExceptionPosition (ZeroDevisionException pos) = pos
getRuntimeExceptionPosition (ZeroModException pos) = pos
getRuntimeExceptionPosition (RangeException pos _ _) = pos
getRuntimeExceptionPosition (ZeroDimensionException pos) = pos


instance Show RuntimeException where 
    show (IndexOutOfRange pos n) = "Index " ++ show n ++ " is out of range."
    show (ZeroDevisionException pos) = "Division by zero."
    show (ZeroModException pos) = "Modulo by zero."
    show (RangeException pos n1 n2) = "Can't create array from range [" ++ show n1 ++ ".." ++ show n2 ++ "]."
    show (ZeroDimensionException pos) = "Can't create array with dimension length zero."

getTypeCheckerExceptionPosition:: TypeCheckerException -> Position
getTypeCheckerExceptionPosition (WrongReturnException pos _ _) = pos
getTypeCheckerExceptionPosition (CantAssignToConstVariable pos ident) = pos
getTypeCheckerExceptionPosition (WrongTypeExceptionBool pos _) = pos
getTypeCheckerExceptionPosition (WrongTypeExceptionTuple pos _) = pos
getTypeCheckerExceptionPosition (WrongTypeExceptionListStr pos _) = pos
getTypeCheckerExceptionPosition (WrongTypeExceptionList pos _) = pos
getTypeCheckerExceptionPosition (WrongTypeExceptionInt pos _) = pos
getTypeCheckerExceptionPosition (WrongTypeException pos _ _) = pos
getTypeCheckerExceptionPosition (NotCallableException pos _) = pos
getTypeCheckerExceptionPosition (WrongNumberOfParametersException pos _ _ _) = pos
getTypeCheckerExceptionPosition (ObjectNotSubscriptableException pos _ _) = pos
getTypeCheckerExceptionPosition (ArrayIndicesNotIntException pos _ _) = pos
getTypeCheckerExceptionPosition (NoSuchVariableOrFunctionException pos _) = pos
getTypeCheckerExceptionPosition (NotSimpleTypeException pos _) = pos
getTypeCheckerExceptionPosition (WrongLengthTupleAssignmentException pos _ _) = pos
getTypeCheckerExceptionPosition (ReturnOutsideFunction pos) = pos
getTypeCheckerExceptionPosition (BreakOutsideLoop pos) = pos
getTypeCheckerExceptionPosition (ContinueOutsideLoop pos) = pos
getTypeCheckerExceptionPosition (DoubleDeclarationException pos _ _) = pos
getTypeCheckerExceptionPosition (IndexOutOfDimension pos) = pos
getTypeCheckerExceptionPosition (WrongTypeAddException pos _ _) = pos

instance Show TypeCheckerException where 
    show (WrongReturnException pos t1 t2) = "Wrong return type.\nShould be " ++ printTree t1 ++ ", was "++ printTree t2  ++ "."
    show (CantAssignToConstVariable pos ident) = "Can't assign value to const variable '" ++ printTree ident ++ "'."
    show (WrongTypeExceptionBool pos t) = "Expected a boolean expression.\nInstead got " ++ printTree t  ++ "."
    show (WrongTypeExceptionTuple pos t) = "Expected a tuple expression.\nInstead got " ++ printTree t  ++ "."
    show (WrongTypeExceptionListStr pos t) = "Expected a list or string expression.\nInstead got " ++ printTree t  ++ "."
    show (WrongTypeExceptionList pos t) = "Expected a list expression.\nInstead got " ++ printTree t  ++ "."
    show (WrongTypeExceptionInt pos t) = "Expected an int expression.\nInstead got " ++ printTree t ++ "."
    show (DoubleDeclarationException pos ident pos2) = "Name '" ++ printTree ident ++ "' was already used in this block in line " ++ show line ++ ", column " ++ show column ++ "."
        where (BNFC'Position line column) = pos2 
    show (WrongTypeException pos t1 t2) = "Wrong type!\nExpected " ++ printTree t1 ++ ".\nInstead got " ++ printTree t2 ++ "."
    show (NotCallableException pos ident) = printTree ident ++ " is not a function, it cannot be called."
    show (WrongNumberOfParametersException pos ident n1 n2) = "Function '" ++ printTree ident ++ "' expected " ++ show n1 ++ " parameters.\nInstead got " ++ show n2 ++ "."
    show (ObjectNotSubscriptableException pos t1 e) = "Expression '" ++ printTree e ++ "' is of type " ++ printTree t1 ++ " , it is not subscriptable."
    show (ArrayIndicesNotIntException pos t e) = "List indices must be of type int. Expression '" ++ printTree e ++ "' is of type " ++ printTree t ++ " not int."
    show (NoSuchVariableOrFunctionException pos ident) = "There is no function or variable named '" ++ printTree ident ++ "'."
    show (NotSimpleTypeException pos t) = "This expression should be a simple type (int, string, bool) not " ++ printTree t ++ "."
    show (WrongLengthTupleAssignmentException pos n1 n2) = "There is wrong length of tuple, expected " ++ show n1 ++ " found " ++ show n2 ++ "."
    show (ReturnOutsideFunction pos) = "Found return outside function."
    show (BreakOutsideLoop pos) = "Found break outside loop."
    show (ContinueOutsideLoop pos) = "Found continue outside loop."
    show (IndexOutOfDimension pos) = "Too many brackets iterable object run out of dimensions."
    show (WrongTypeAddException pos t1 t2) = "Add can only be performed on two ints or two strings, not " ++ printTree t1 ++ " and " ++ printTree t2 ++ "."