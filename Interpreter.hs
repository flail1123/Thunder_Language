module Interpreter where 
import Prelude as P
import AbsGrammar
import Types
import Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Data.Maybe

runInterpreter:: Program -> IO (Either RuntimeException ())
runInterpreter prog = runExceptT $ evalStateT (runReaderT (interpret prog) M.empty) (M.empty)

interpret:: Program -> InterpreterMonad() 
interpret (Program pos stmts) = do 
    evalBlock (Block pos stmts)
    return ()

getNewLoc:: InterpreterMonad(Loc)
getNewLoc = do 
    store <- get 
    return $ toInteger $ length store

returnCurrentEnvI:: InterpreterMonad(EnvI)
returnCurrentEnvI = do 
    env <- ask 
    return env

getIdentLoc:: Ident -> InterpreterMonad(Loc) 
getIdentLoc ident = do 
    env <- ask 
    return $ fromJust $ M.lookup ident env 


getIdentValue:: Ident -> InterpreterMonad(Value) 
getIdentValue ident = do 
    store <- get 
    loc <- getIdentLoc ident 
    return $ fromJust $ M.lookup loc store 

declareIdents:: [Ident] -> [Value] -> InterpreterMonad(EnvI)
declareIdents (ident:idents) (value:values) = do 
    newEnv <- declareIdent ident value 
    local (const newEnv) (declareIdents idents values)

declareIdents [] [] = do 
    env <- ask 
    return env

addListToStore:: Value -> InterpreterMonad(Value)
addListToStore value = do 
    loc <- getNewLoc
    store <- get 
    put $ M.insert loc value store
    return $ LocV loc

declareIdent:: Ident -> Value -> InterpreterMonad(EnvI)
declareIdent ident value = do 
    loc <- getNewLoc 
    env <- ask 
    store <- get 
    put $ M.insert loc value store
    return $ M.insert ident loc env 

getValueFromLoc:: Loc -> InterpreterMonad(Value) 
getValueFromLoc loc = do 
    store <- get 
    return $ fromJust $ M.lookup loc store 

changeLocValue:: Loc -> Value -> InterpreterMonad() 
changeLocValue loc value = do 
    store <- get 
    put $ M.insert loc value store 


changeIdentValue:: Ident -> Value -> InterpreterMonad(EnvI)
changeIdentValue ident value = do 
    loc <- getIdentLoc ident
    env <- ask 
    store <- get 
    put $ M.insert loc value store
    return env 

changeIdentValues:: [Ident] -> [Value] -> InterpreterMonad(EnvI)
changeIdentValues (i:is) (v:vs) = do 
    newEnv <- changeIdentValue i v 
    local (const newEnv) (changeIdentValues is vs)

changeIdentValues [] [] = returnCurrentEnvI


evalMulOp:: Integer -> MulOp -> Integer -> InterpreterMonad(Value) 
evalMulOp n1 (Times pos) n2 = do 
    return $ IntV $ n1 * n2 

evalMulOp n1 (Div pos) n2 = do 
    case n2 of 
        0 -> throwError $ ZeroDevisionException pos
        otherwise -> do 
            return $ IntV $ n1 `div` n2 

evalMulOp n1 (Mod pos) n2 = do 
    case n2 of 
        0 -> throwError $ ZeroModException pos
        otherwise -> do 
            return $ IntV $ n1 `mod` n2 

evalRelOp:: Integer -> RelOp -> Integer -> InterpreterMonad(Value) 
evalRelOp n1 (LTH pos) n2 = do 
    return $ BoolV $ n1 < n2 

evalRelOp n1 (LE pos) n2 = do 
    return $ BoolV $ n1 <= n2 

evalRelOp n1 (GTH pos) n2 = do 
    return $ BoolV $ n1 > n2 

evalRelOp n1 (GE pos) n2 = do 
    return $ BoolV $ n1 >= n2 

evalAddOp:: Integer -> AddOp -> Integer -> InterpreterMonad(Value) 
evalAddOp n1 (Plus pos) n2 = do 
    return $ IntV $ n1 + n2 

evalAddOp n1 (Minus pos) n2 = do 
    return $ IntV $ n1 - n2 



evalEqOp:: Value -> EqOp -> Value -> InterpreterMonad(Value) 
evalEqOp c1 (EQU pos) c2 = do 
    return $ BoolV $ c1 == c2 

evalEqOp c1 (NE pos) c2 = do 
    return $ BoolV $ not $ c1 == c2 


getDefaultValue:: Type -> InterpreterMonad(Value) 
getDefaultValue (IntT _) = do return $ IntV 0 
getDefaultValue (StrT _) = do return $ StrV "" 
getDefaultValue (BoolT _) = do return $ BoolV False  
getDefaultValue (TupleT pos []) = do return $ TupleV []     
getDefaultValue (TupleT pos (t:ts)) = do
    (TupleV values) <- getDefaultValue (TupleT pos ts)
    value <- getDefaultValue t 
    return $ TupleV (value:values)
 
        
getDefaultValue (ListT pos t) = do
    loc <- getNewLoc
    store <- get 
    defValue <- getDefaultValue t
    put $ M.insert loc (ListV [defValue]) store
    return $ LocV loc


giveNObjects:: Integer -> (Position, Type, [Expr]) -> InterpreterMonad([Value])
giveNObjects 0 (pos, _, _) = throwError $ ZeroDimensionException pos 
giveNObjects 1 (pos, t, exprs) = do 
    v <- evalExprList (AllocList pos t exprs)
    return [v] 

giveNObjects n (pos, t, exprs) = do 
    v <- evalExprList (AllocList pos t exprs)
    vs <- giveNObjects (n-1) (pos, t, exprs)
    return (v:vs)

evalExprList:: ExprList -> InterpreterMonad(Value)
evalExprList (EList pos exprs) = do 
    values <- evalExpressions exprs 
    result <- addListToStore (ListV values)
    return result

evalExprList (AllocList pos t (expr:exprs)) = do 
    (IntV n) <- evalExpression expr
    value <- giveNObjects n (pos, t, exprs)
    result <- addListToStore (ListV value)
    return result

evalExprList (AllocList pos t []) = do 
    getDefaultValue t

evalExprList (ERange pos expr1 expr2) = do 
    (IntV n1) <- evalExpression expr1 
    (IntV n2) <- evalExpression expr2 
    case n1 < n2 of 
        True -> do
            result <- addListToStore $ ListV (P.map (\n -> (IntV n))[n1 .. (n2-1)])
            return $ result
        False -> throwError $ RangeException pos n1 n2

evalExpressions:: [Expr] -> InterpreterMonad([Value])
evalExpressions [] = do 
    return []

evalExpressions (e:es) = do 
    v <- evalExpression e 
    vs <- evalExpressions es
    return (v:vs)

evalExpression:: Expr -> InterpreterMonad(Value) 
evalExpression (NoneExpr pos) = do 
    return NoneV 

evalExpression (AnyListExpr pos exprList) = do 
    evalExprList exprList
    

evalExpression (Access pos expr1 expr2) = do
    v <- evalExpression expr1
    (IntV n) <- evalExpression expr2 
    case v of 
        (LocV loc) -> do 
            (ListV values) <- getValueFromLoc loc 
            case n >= toInteger(length values) of 
                True -> throwError $ IndexOutOfRange pos n
                False -> return $ values !! (fromInteger n)  
        (StrV s) -> do 
            case n >= toInteger(length s) of 
                True -> throwError $ IndexOutOfRange pos n
                False -> return $ StrV [s !! (fromInteger n)] 

evalExpression (EApp pos ident exprs) = do 
    (FunV _ envF args block) <- getIdentValue ident 
    values <- evalExpressions exprs 
    newFEnv <- local (const envF) (declareIdents args values)
    (Return v) <- local (const newFEnv) (evalBlock block)
    return v 

evalExpression (EAppEmpty pos ident) = evalExpression (EApp pos ident [])

evalExpression (Len pos expr) = do 
    v <- evalExpression expr 
    case v of 
        (StrV s) -> return $ IntV $ toInteger $ length s 
        (LocV loc) -> do 
            (ListV vs) <- getValueFromLoc loc 
            return $ IntV $ toInteger $ length vs 

evalExpression (TupleExpr pos expr exprs) = do 
    values <- evalExpressions (expr:exprs)
    return $ TupleV values 

evalExpression (EVar pos ident) = do 
    value <- getIdentValue ident 
    return value 

evalExpression (ELitInt pos n) = do 
    return $ IntV n 

evalExpression (ELitTrue pos) = do 
    return $ BoolV True 

evalExpression (ELitFalse pos) = do 
    return $ BoolV False 

evalExpression (EString pos s) = do 
    return $ StrV s 

evalExpression (Neg pos expr) = do 
    (IntV n) <- evalExpression expr 
    return $ IntV (-n)

evalExpression (Not pos expr) = do 
    (BoolV b) <- evalExpression expr 
    return $ BoolV $ not b 

evalExpression (EMul pos expr1 mulOp expr2) = do 
    (IntV n1) <- evalExpression expr1 
    (IntV n2) <- evalExpression expr2 
    evalMulOp n1 mulOp n2 

evalExpression (EAdd pos expr1 addOp expr2) = do 
    t1 <- evalExpression expr1 
    t2 <- evalExpression expr2 
    case (t1, t2) of 
        (IntV n1, IntV n2) -> evalAddOp n1 addOp n2 
        (StrV s1, StrV s2) -> do 
            return $ StrV $ s1 ++ s2

evalExpression (ERel pos expr1 relOp expr2) = do 
    (IntV n1) <- evalExpression expr1 
    (IntV n2) <- evalExpression expr2 
    evalRelOp n1 relOp n2 

evalExpression (EAnd pos expr1 expr2) = do 
    (BoolV b1) <- evalExpression expr1 
    (BoolV b2) <- evalExpression expr2 
    return $ BoolV $ b1 && b2
    
evalExpression (EOr pos expr1 expr2) = do 
    (BoolV b1) <- evalExpression expr1 
    (BoolV b2) <- evalExpression expr2 
    return $ BoolV $ b1 || b2

evalExpression (EEq pos expr1 eqOp expr2) = do 
    v1 <- evalExpression expr1 
    v2 <- evalExpression expr2 
    evalEqOp v1 eqOp v2 


evalBlock:: Block -> InterpreterMonad(Flag)
evalBlock (Block pos (stmt:stmts)) = do 
    (flag, newEnv) <- evalStmt stmt 
    case flag of 
        Empty -> do 
            local (const newEnv) (evalBlock (Block pos stmts))
        otherwise -> return flag 

evalBlock (Block pos []) = do
    return Empty

evalOpIdent:: Position -> Ident -> Expr -> (Integer -> Integer -> Integer) -> InterpreterMonad((Flag, EnvI))
evalOpIdent pos ident expr op = do 
    (IntV n1) <- getIdentValue ident 
    (IntV n2) <- evalExpression expr
    newEnv <- changeIdentValue ident (IntV (op n1 n2))
    return (Empty, newEnv) 

doNothing:: InterpreterMonad((Flag, EnvI))
doNothing = evalStmt (Skip Nothing) 

evalFor:: Ident -> [Value] -> Block -> InterpreterMonad(Flag, EnvI)
evalFor ident (value:values) block = do 
    env <- ask 
    newEnv <- changeIdentValue ident value 
    flag <- local (const newEnv) (evalBlock block) 
    case flag of 
        Empty -> local (const newEnv) (evalFor ident values block) 
        Continue -> local (const newEnv) (evalFor ident values block) 
        Break -> do 
            return (Empty, env)
        _ -> do
            return (flag, env)

evalFor ident [] block = doNothing 

addIdentGiveLoc:: Ident -> InterpreterMonad((EnvI, Loc))
addIdentGiveLoc ident = do
    env <- ask 
    loc <- getNewLoc
    return (M.insert ident loc env, loc)

evalStmt:: Stmt -> InterpreterMonad((Flag, EnvI))
evalStmt (FnDef pos t ident args (Block pos2 stmts)) = do 
    (newEnv, loc) <- addIdentGiveLoc ident
    store <- get 
    put $ M.insert loc (FunV t newEnv (P.map (\(Arg p t i)-> i) args) (Block pos2 (stmts ++ [(Ret Nothing (NoneExpr pos2))]))) store
    
    return (Empty, newEnv)

evalStmt (Skip pos) = do
    env <- ask 
    return (Empty, env)

evalStmt (Decl pos t ((Init pos2 ident expr):items)) = do 
    env <- ask 
    value <- evalExpression expr 
    newEnv <- declareIdent ident value 
    local (const newEnv) (evalStmt(Decl pos t items))

evalStmt (Decl pos t ((NoInit pos2 ident):items)) = do
    env <- ask  
    defValue <- getDefaultValue t
    newEnv <- declareIdent ident defValue  
    local (const newEnv) (evalStmt(Decl pos t items))

evalStmt (Decl pos t []) = doNothing

evalStmt (CDecl pos t items) = evalStmt (Decl pos t items)

evalStmt (BStmt pos block) = do
    env <- ask 
    flag <- evalBlock block
    return (flag, env)

evalStmt (Incr pos ident) = do 
    (IntV n) <- getIdentValue ident 
    newEnv <- changeIdentValue ident (IntV (n+1))
    return (Empty, newEnv) 

evalStmt (Decr pos ident) = do 
    (IntV n) <- getIdentValue ident 
    newEnv <- changeIdentValue ident (IntV (n-1))
    return (Empty, newEnv) 

evalStmt (MulIdent pos ident expr) = evalOpIdent pos ident expr (*)

evalStmt (SubIdent pos ident expr) = evalOpIdent pos ident expr (-)

evalStmt (AddIdent pos ident expr) = evalOpIdent pos ident expr (+)

evalStmt (DivIdent pos ident expr) = do
    (IntV n2) <- evalExpression expr
    case n2 == 0 of 
        False -> evalOpIdent pos ident expr (div)
        True -> throwError $ ZeroDevisionException pos

evalStmt (ModIdent pos ident expr) = do 
    (IntV n2) <- evalExpression expr
    case n2 == 0 of 
        False -> evalOpIdent pos ident expr (mod)
        True -> throwError $ ZeroModException pos

evalStmt (Ret pos expr) = do 
    env <- ask 
    value <- evalExpression expr 
    return $ (Return value, env)

evalStmt (VRet pos) = evalStmt (Ret pos (NoneExpr pos))

evalStmt (CondElse pos expr block1 block2) = do 
    (BoolV c) <- evalExpression expr
    env <- ask
    case c of 
        True -> do 
            flag <- evalBlock block1 
            return (flag, env)
        False -> do 
            flag <- evalBlock block2 
            return (flag, env)

evalStmt (Cond pos expr block) = evalStmt (CondElse pos expr block (Block pos [(Skip pos)]))

evalStmt (While pos expr block) = do 
    (BoolV c) <- evalExpression expr 
    env <- ask
    case c of 
        False -> doNothing 
        True -> do 
            flag <- evalBlock block 
            case flag of 
                Empty -> local (const env) (evalStmt (While pos expr block)) 
                Continue -> local (const env) (evalStmt (While pos expr block)) 
                Break -> do 
                    return (Empty, env)
                _ -> do
                    return (flag, env)

evalStmt (For pos (Arg pos2 t ident) expr block) = do
    v2 <- evalExpression expr
    defValue <- getDefaultValue t
    case v2 of 
        (LocV loc) -> do
            (ListV values) <- getValueFromLoc loc
            newEnv <- declareIdent ident defValue
            local (const newEnv) (evalFor ident values block)
        (StrV s) -> do
            newEnv <- declareIdent ident defValue
            local (const newEnv) (evalFor ident (P.map (\c -> StrV [c]) s) block)

evalStmt (For pos (CArg pos2 t ident) expr block) = evalStmt (For pos (Arg pos2 t ident) expr block)

evalStmt (BreakStmt pos) = do 
    env <- ask 
    return (Break, env)

evalStmt (ContinueStmt pos) = do 
    env <- ask 
    return (Continue, env)

evalStmt (Print pos exprs) = do 
    values <- evalExpressions exprs 
    s <- (printValues values " ")
    liftIO $ putStrLn $ s
    doNothing

evalStmt (SApp pos ident exprs) = do 
    env <- ask 
    values <- evalExpressions exprs
    (FunV _ envF idents block) <- getIdentValue ident 
    newEnvF <- local (const envF) (declareIdents idents values)
    local (const newEnvF) (evalBlock block)
    return (Empty, env)

evalStmt (SAppEmpty pos ident) = evalStmt (SApp pos ident [])

evalStmt (TupleAss pos (TupleIdents pos2 item items) expr) = do
    env <- ask 
    (TupleV values) <- evalExpression expr 
    doTupleAss (item:items) values env

evalStmt (Ass pos ident bracketExprs expr) = do 
    indices <- evalExpressions (P.map (\(BracketE _ e) -> e) bracketExprs)
    value <- evalExpression expr
    oldValue <- getIdentValue ident 
    newIdentValue <- doAss pos indices oldValue value
    newEnv <- changeIdentValue ident (newIdentValue)
    return (Empty, newEnv) 


doAss:: Position -> [Value] -> Value -> Value -> InterpreterMonad(Value)
doAss pos [] iterableValue value = do
    return value

doAss pos ((IntV n):indices) iterableValue value = do
    case iterableValue of 
        (LocV loc) -> do 
            (ListV values) <- getValueFromLoc loc
            case toInteger(length values) <= n of 
                True -> throwError $ IndexOutOfRange pos n
                False -> do
                    newValue <- doAss pos indices (values !! (fromInteger n)) value
                    changeLocValue loc (ListV $ replace n newValue values)
                    return (LocV loc)
        (StrV s) -> do 
            case toInteger(length s) <= n of 
                True -> throwError $ IndexOutOfRange pos n
                False -> return $ StrV $ replace n ((getStrFromStrV value) !! 0) s

getStrFromStrV (StrV s) = s

replace _ _ [] = []
replace 0 a (_:ys) = a:ys
replace n a (y:ys) =
  if n < 0
    then (y:ys)
    else y: replace (n-1) a ys

getValuesFromTuple:: Value -> [Value]
getValuesFromTuple (TupleV values) = values

doTupleAss:: [TupleItem] -> [Value] -> EnvI -> InterpreterMonad(Flag, EnvI) 
doTupleAss (item:items) (value:values) envTuple = do 
    env <- ask 
    case item of 
        (TupleItem pos ident) -> do 
            newEnv <- local (const env) (changeIdentValue ident value) 
            local (const newEnv) (doTupleAss items values env)
        (TupleItemU _) -> doTupleAss items values envTuple
        (TupleItemRek _ (TupleIdents _ newItem newItems)) -> do
            
            (_, newEnv) <- doTupleAss (newItem:newItems) (getValuesFromTuple value) env 
            local (const newEnv) (doTupleAss items values env)         

doTupleAss [] [] envTuple = do 
    env <- ask 
    return (Empty, env)

printValue:: Value -> InterpreterMonad(String)
printValue value = do 
    case value of 
        (NoneV) -> do 
            return "None"
        (IntV n) -> do 
            return $ show n
        (StrV s) -> do 
            return s
        (BoolV c) -> do 
            return $ show c
        (TupleV values) -> do
            s <- printValues values ", "
            return $ "(" ++ s ++ ")"
        (LocV loc) -> do
            (ListV values) <- getValueFromLoc loc
            s <- printValues values ", "
            return $ "[" ++ s ++ "]"
        
printValues:: [Value] -> String -> InterpreterMonad(String)

printValues (value:[]) sep = do
    s <- printValue value 
    return $ s

printValues (value:values) sep = do
    s <- printValue value 
    s2 <- printValues values sep 
    return $ s ++ sep ++ s2

printValues [] sep = do 
    return ""
        

