module TypeChecker where 
import Prelude as P
import AbsGrammar
import Types
import Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except


runTypeChecker:: Program -> Either TypeCheckerException ()
runTypeChecker prog = runIdentity $ runExceptT $ runReaderT (checkTypes prog) M.empty 

resultFunctionValueString = "*resultFunctionValue"

isInsideLoopString = "*isInsideLoop"

declare:: Position -> Bool -> Type  -> Ident-> TypeCheckerMonad(EnvT)
declare pos b t name = do
    env <- ask
    return $ M.insert name (b, t) env

getIdentType:: Ident -> Position -> TypeCheckerMonad(Type)
getIdentType ident pos = do 
    (c, t) <- getIdentInfo ident pos
    return t

getIdentInfo:: Ident -> Position -> TypeCheckerMonad((Bool, Type))
getIdentInfo ident pos = do
    env <- ask
    case (M.lookup ident env) of 
        Just(info) -> return info 
        Nothing -> throwError $ NoSuchVariableOrFunctionException pos ident 


returnCurrentEnvT:: TypeCheckerMonad(EnvT)
returnCurrentEnvT = do 
    env <- ask 
    return env

checkTypes:: Program -> TypeCheckerMonad ()
checkTypes (Program pos statements) = do 
    newEnvT <- asks (M.insert (Ident resultFunctionValueString) (False, NoneT Nothing))
    newNewEnvT <- local (const newEnvT) (asks (M.insert (Ident isInsideLoopString) (False, NoneT Nothing)))
    local (const newNewEnvT) (checkStatements statements)

checkStatements:: [Stmt] -> TypeCheckerMonad ()
checkStatements (stmt:stmts) = do
    newEnvT <- checkStatement stmt
    local (const newEnvT) (checkStatements stmts)

checkStatements [] =
    return () 

getIdentFromStmt:: Stmt -> Maybe (Ident, Position, Stmt) 
getIdentFromStmt (FnDef pos _ ident _ _) = Just(ident, pos, (Skip pos)) 
getIdentFromStmt (Decl pos t ((NoInit pos2 ident):items)) = Just((ident, pos2, (Decl pos t items))) 
getIdentFromStmt (Decl pos t ((Init pos2 ident _):items)) = Just((ident, pos2, (Decl pos t items))) 
getIdentFromStmt (CDecl pos t ((NoInit pos2 ident):items)) = Just((ident, pos2, (CDecl pos t items))) 
getIdentFromStmt (CDecl pos t ((Init pos2 ident _):items)) = Just((ident, pos2, (CDecl pos t items))) 
getIdentFromStmt stmt = Nothing

checkForDoubleDeclarations:: Map Ident Position -> [Stmt] -> TypeCheckerMonad() 
checkForDoubleDeclarations m (stmt:stmts) = do 
    case (getIdentFromStmt stmt ) of 
        Nothing -> checkForDoubleDeclarations m stmts
        Just((ident, pos, newStmt)) -> do 
            case (M.lookup ident m) of 
                Just(pos2) -> throwError $ DoubleDeclarationException pos ident pos2
                Nothing -> checkForDoubleDeclarations (M.insert ident pos m) (newStmt:stmts)
checkForDoubleDeclarations m [] = do 
    return ()

checkBlock:: Block -> TypeCheckerMonad ()
checkBlock (Block pos stmts) = do
    checkForDoubleDeclarations (M.empty) (stmts)
    checkStatements stmts
    return ()

declareArgs:: [Arg] -> TypeCheckerMonad(EnvT)
declareArgs ((Arg p t i):args) = do 
    env <- ask 
    newEnvT <- declare p False t i 
    local (const newEnvT) (declareArgs args)

declareArgs ((CArg p t i):args) = do 
    env <- ask 
    newEnvT <- declare p True t i 
    local (const newEnvT) (declareArgs args)

declareArgs [] = do 
    returnCurrentEnvT

isTypeBool:: Type -> Position -> TypeCheckerMonad()
isTypeBool t pos = do
    case t of 
        (BoolT _) -> return ()
        otherwise -> throwError $ WrongTypeExceptionBool pos t 

isTypeInt:: Type -> Position -> TypeCheckerMonad()
isTypeInt t pos = do
    case t of 
        (IntT _) -> return ()
        otherwise -> throwError $ WrongTypeExceptionInt pos t  

isExprBool:: Expr -> TypeCheckerMonad()
isExprBool e = do 
    t <- getExprType e 
    isTypeBool t (hasPosition e)

isExprInt:: Expr -> TypeCheckerMonad()
isExprInt e = do 
    t <- getExprType e 
    isTypeInt t (hasPosition e)

areExprsInt:: [Expr] -> TypeCheckerMonad()
areExprsInt (e:es) = do 
    isExprInt e 
    areExprsInt es 
areExprsInt [] = do 
    return ()



getListExprType:: ExprList -> TypeCheckerMonad(Type) 
getListExprType (AllocList pos t exprs) = do 
    areExprsInt exprs
    case t of 
        (ListT p _) -> do
            throwError $ WrongTypeExceptionList pos t
        (FunT p _ _) -> do
            throwError $ WrongTypeExceptionList pos t
        otherwise -> do
            return $ ListT pos t

getListExprType (EList pos (expr:exprs)) = do
    t <- getExprType expr
    ts <- getExprsTypes (expr:exprs)
    typesEqual (P.map (const t) [1..(length ts)]) $ ts
    return $ ListT pos t

getListExprType (ERange pos e1 e2) = do
    isExprInt e1
    isExprInt e2
    return $ ListT pos (IntT pos)


getExprsTypes:: [Expr] -> TypeCheckerMonad([Type])
getExprsTypes (e:es) = do 
    t <- getExprType e 
    ts <- getExprsTypes es
    return (t:ts)
getExprsTypes [] = do 
    return []


getExprType:: Expr -> TypeCheckerMonad (Type)

getExprType (Len pos expr) = do 
    v <- getExprType expr 
    case v of 
        (ListT pos2 _) -> return (IntT pos) 
        (StrT pos2) -> return (IntT pos)
        otherwise -> throwError $ WrongTypeExceptionListStr pos v

getExprType (AnyListExpr pos listExpr) = do 
    getListExprType listExpr 

getExprType (Access pos expr1 expr2) = do 
    type1 <- getExprType expr1
    type2 <- getExprType expr2  
    case type2 of 
        (IntT n) -> do
            case type1 of 
                (ListT pos inerType) -> return inerType 
                (StrT pos) -> return type1 
                otherwise -> throwError $ ObjectNotSubscriptableException (hasPosition type1) type1 expr1 
        otherwise -> throwError $ ArrayIndicesNotIntException (hasPosition type2) type2 expr2
    

getExprType (EApp pos ident exprs) = do
    exprTypes <- getExprsTypes exprs
    t <- (getIdentType ident pos)
    case t of 
        (FunT pos2 returnType argTypes) -> do
            case (length argTypes == length exprTypes) of
                True -> do
                    typesEqual argTypes exprTypes 
                    return $ returnType
                otherwise -> throwError $ WrongNumberOfParametersException pos ident (length argTypes) (length exprTypes)
        otherwise -> throwError $ NotCallableException pos ident


getExprType (EAppEmpty pos ident) = getExprType (EApp pos ident [])

getExprType (TupleExpr pos expr exprs) = do
    ts <- getExprsTypes (expr:exprs)
    return $ TupleT pos ts

getExprType (EVar pos ident) = do 
    getIdentType ident pos

getExprType (ELitInt pos n) = do 
    return $ IntT pos 

getExprType (ELitTrue pos) = do 
    return $ BoolT pos 

getExprType (ELitFalse pos) = do 
    return $ BoolT pos 

getExprType (EString pos _) = do 
    return $ StrT pos 

getExprType (Not pos expr) = do 
    isExprBool expr 
    return $ BoolT pos 

getExprType (Neg pos expr) = do 
    isExprInt expr
    return $ IntT pos 

getExprType (EMul pos expr1 _ expr2) = do 
    isExprInt expr1
    isExprInt expr2
    return $ IntT pos 

getExprType (EAdd pos expr1 (Minus _) expr2) = getExprType $EMul pos expr1 (Times pos) expr2

getExprType (EAdd pos expr1 (Plus _) expr2) = do 
    t1 <- getExprType expr1 
    t2 <- getExprType expr2 
    case (t1,t2) of 
        (IntT _, IntT _) -> return (IntT pos) 
        (StrT _, StrT _) -> return (StrT pos) 
        otherwise -> throwError $ WrongTypeAddException pos t1 t2

getExprType (ERel pos expr1 _ expr2) = do 
    isExprInt expr1
    isExprInt expr2
    return $ BoolT pos 

getExprType (EEq pos expr1 _ expr2) = do 
    t1 <- getExprType expr1
    t2 <- getExprType expr2
    typesEqual [t1] [t2]
    isSimpleType t1 (hasPosition expr1)
    return $ BoolT pos 

getExprType (EAnd pos expr1 expr2) = do
    isExprBool expr1
    isExprBool expr2
    return $ BoolT pos 

getExprType (EOr pos expr1 expr2) = getExprType (EAnd pos expr1 expr2)

getExprType (NoneExpr pos) = do 
    return $ NoneT pos

isSimpleType:: Type -> Position -> TypeCheckerMonad()
isSimpleType t pos = do 
    case t of 
        (IntT _) -> do
            return ()
        (StrT _) -> do
            return ()
        (BoolT _) -> do
            return ()
        (NoneT _) -> do
            return ()
        otherwise -> do
            throwError $ NotSimpleTypeException pos t

typesEqual:: [Type] -> [Type] -> TypeCheckerMonad()
typesEqual [] [] = do 
    return ()
typesEqual (x:xs) (y:ys) = do 
    case (x, y) of 
        ((IntT _ ), (IntT _)) -> typesEqual xs ys
        ((IntT _ ), _) -> throwError $ WrongTypeException (hasPosition y) x y 

        ((StrT _ ), (StrT _)) -> typesEqual xs ys
        ((StrT _ ), _) -> throwError $ WrongTypeException (hasPosition y) x y

        ((BoolT _ ), (BoolT _)) -> typesEqual xs ys
        ((BoolT _ ), _) -> throwError $ WrongTypeException (hasPosition y) x y 

        ((NoneT _ ), (NoneT _)) -> typesEqual xs ys
        ((NoneT _ ), _) -> throwError $ WrongTypeException (hasPosition y) x y 

        ((ListT _ _), (ListT _ _)) -> typesEqual xs ys
        ((ListT _ _), _) -> throwError $ WrongTypeException (hasPosition y) x y 

        ((TupleT pos1 t1 ), (TupleT pos2 t2)) -> do
            case length t1 == length t2 of 
                True -> do 
                    typesEqual t1 t2
                    typesEqual xs ys
                False -> throwError $ WrongLengthTupleAssignmentException pos2 (length t1) (length t2)
            typesEqual t1 t2
            typesEqual xs ys
        ((TupleT _ _ ), _) -> throwError $ WrongTypeException (hasPosition y) x y  

        ((FunT _ rt1 pt1), (FunT _ rt2 pt2)) -> do
            typesEqual (rt1:pt1) (rt2:pt2)
            typesEqual xs ys
        ((FunT _ _ _ ), _) -> throwError $ WrongTypeException (hasPosition y) x y  




doDeclaration:: Bool -> Type -> Item -> TypeCheckerMonad (EnvT)
doDeclaration b t (Init pos ident expr) = do 
    env <- ask 
    exprType <- getExprType expr 
    typesEqual [t] [exprType]
    declare pos b t ident 

doDeclaration b t (NoInit pos ident) = do 
    declare pos b t ident 

maybeAddReturnNone:: [Stmt] -> Position -> TypeCheckerMonad(Block)
maybeAddReturnNone stmts pos = do 
    case stmts of 
        [] -> do
            return (Block pos (stmts ++ [VRet pos]))
        otherwise -> do 
            case (last stmts) of 
                (Ret _ _) -> do
                    return (Block pos stmts)
                (VRet _ ) -> do
                    return (Block pos stmts)
                otherwise -> do
                    return (Block pos (stmts ++ [VRet pos]))

isInsideLoop:: Flag -> Position -> TypeCheckerMonad() 
isInsideLoop s pos = do 
    env <- ask 
    (isInsideLoop, _) <- getIdentInfo (Ident isInsideLoopString) pos
    case isInsideLoop of 
        False -> do
            case s of 
                Break -> throwError $ BreakOutsideLoop pos
                Continue -> throwError $ ContinueOutsideLoop pos
        True -> do 
            return ()

isAssOk:: Position -> Type -> [Expr] -> Type -> TypeCheckerMonad()
isAssOk pos iterableT (be:bes) t = do 
    isExprInt be 
    case iterableT of 
        (ListT pos2 t2) -> 
            isAssOk pos t2 bes t
        (StrT pos2) -> 
            case length bes of 
                0 -> do 
                    typesEqual [StrT pos2] [t] 
                    return ()
                otherwise -> throwError $ IndexOutOfDimension pos 
        otherwise -> throwError $ IndexOutOfDimension pos 

isAssOk pos t2 [] t = do
    typesEqual [t2] [t] 
    return ()

checkStatement:: Stmt -> TypeCheckerMonad (EnvT)
checkStatement (FnDef pos resultType name args (Block pos2 stmts)) = do
    newEnvT <- declare pos True (FunT pos resultType (P.map (\(Arg p t i)-> t) args)) name
    newNewEnvT <- local (const newEnvT) (declareArgs args)
    block <- maybeAddReturnNone stmts pos2 
    local (const (M.insert (Ident resultFunctionValueString) (True, resultType) newNewEnvT)) (checkBlock block)
    return newEnvT

checkStatement (Skip pos) = do 
    returnCurrentEnvT

checkStatement (BStmt pos b) = do
    env <- ask 
    checkBlock b
    return env 

checkStatement (Decl pos t (item:items)) = do
    env <- ask 
    newEnvT <- local (const env) (doDeclaration False t item)
    local (const newEnvT) $ checkStatement (Decl pos t items)

checkStatement (Decl pos t []) = do
    env <- ask 
    return env

checkStatement (CDecl pos t (item:items)) = do
    env <- ask 
    newEnvT <- local (const env) (doDeclaration True t item)
    local (const newEnvT) $ checkStatement (Decl pos t items)

checkStatement (CDecl pos t []) = do
    env <- ask 
    return env

checkStatement (Incr pos ident) = do 
    t <- getIdentType ident pos 
    isTypeInt t pos
    returnCurrentEnvT

checkStatement (Decr pos ident) = do 
    t <- getIdentType ident pos
    isTypeInt t pos
    returnCurrentEnvT

checkStatement (MulIdent pos ident e) = do 
    (isConst, t1) <- getIdentInfo ident pos
    case isConst of 
        True -> throwError $ CantAssignToConstVariable pos ident
        False -> do
            isTypeInt t1 pos
            isExprInt e
            returnCurrentEnvT

checkStatement (SubIdent pos ident e) = checkStatement (MulIdent pos ident e)

checkStatement (AddIdent pos ident e) = checkStatement (MulIdent pos ident e)

checkStatement (DivIdent pos ident e) = checkStatement (MulIdent pos ident e)

checkStatement (ModIdent pos ident e) = checkStatement (MulIdent pos ident e)

checkStatement (Ret pos expr) = do 
    t2 <- getExprType expr
    (isInsideFunction, t1) <- getIdentInfo (Ident resultFunctionValueString) pos
    case isInsideFunction of 
        False -> throwError $ ReturnOutsideFunction pos 
        True -> do 
            catchError (typesEqual [t1] [t2]) (\_ -> throwError $ WrongReturnException pos t1 t2)
            env <- ask 
            return env

checkStatement (VRet pos) = checkStatement (Ret pos (NoneExpr pos))

checkStatement (Cond pos e b) = do 
    isExprBool e 
    checkBlock b 
    returnCurrentEnvT

checkStatement (CondElse pos e b1 b2) = do 
    isExprBool e 
    checkBlock b1
    checkBlock b2
    returnCurrentEnvT


checkStatement (While pos e b) = do
    env <- ask 
    isExprBool e 
    local (M.insert (Ident isInsideLoopString) (True, (NoneT Nothing))) (checkBlock b)
    returnCurrentEnvT

checkStatement (For pos arg e b) = do 
    env <- ask
    t <- getExprType e 

    case arg of 
        (Arg p2 t3 ident) -> do
            newNewEnvT <- local (M.insert (Ident isInsideLoopString) (True, (NoneT pos))) (declare p2 False t3 ident)
            local (const newNewEnvT) (checkBlock b)
            case t of 
                (ListT p t2) -> do
                    typesEqual [t2] [t3] 
                    return env
                (StrT p) -> do 
                    typesEqual [(StrT p)] [t3]
                    return env
                otherwise -> throwError $ WrongTypeExceptionList pos t 
        (CArg p2 t3 ident) -> do
            newNewEnvT <- local (M.insert (Ident isInsideLoopString) (True, (NoneT pos))) (declare p2 True t3 ident)
            local (const newNewEnvT) (checkBlock b)
            case t of 
                (ListT p t2) -> do
                    typesEqual [t2] [t3] 
                    return env
                (StrT p) -> do 
                    typesEqual [(StrT p)] [t3]
                    return env
                otherwise -> throwError $ WrongTypeExceptionList pos t 

checkStatement (Ass pos ident bracketExprs expr) = do 
    t <- getExprType expr 
    (isConst, t2) <- getIdentInfo ident pos
    case isConst of 
        True -> throwError $ CantAssignToConstVariable pos ident
        False -> do
            isAssOk pos t2 (P.map (\(BracketE pos e) -> e) bracketExprs) t
            returnCurrentEnvT

checkStatement (BreakStmt pos) = do
    isInsideLoop Break pos 
    returnCurrentEnvT

checkStatement (ContinueStmt pos ) = do
    isInsideLoop Continue pos
    returnCurrentEnvT

checkStatement (Print pos exprs) = do
    getExprsTypes exprs
    returnCurrentEnvT

checkStatement (SApp pos ident exprs) = do
    getExprType (EApp pos ident exprs)
    returnCurrentEnvT

checkStatement (SAppEmpty pos ident) = do
    getExprType (EAppEmpty pos ident)
    returnCurrentEnvT


checkStatement (TupleAss pos (TupleIdents pos2 item items) expr) = do 
    t <- getExprType expr
    checkTupleAssignmentHelper pos (TupleIdents pos2 item items) t

checkTupleAssignmentHelper:: Position -> TupleIdent -> Type -> TypeCheckerMonad(EnvT)
checkTupleAssignmentHelper pos (TupleIdents pos2 item items) t = do 
    case t of 
        (TupleT pos3 types) -> do 
            case length(item:items) == (length types) of 
                True -> checkTupleAssignment pos2 (item:items) types
                False -> throwError $ WrongLengthTupleAssignmentException pos (length (item:items)) (length types)
        otherwise -> throwError $ WrongTypeExceptionTuple (hasPosition t) t 
    

checkTupleAssignment:: Position -> [TupleItem] -> [Type] -> TypeCheckerMonad(EnvT)
checkTupleAssignment pos (item:items) (t:ts) = do
    case item of 
        (TupleItem pos2 ident) -> do
            (c, t1) <- getIdentInfo ident pos2
            case c of 
                True -> throwError $ CantAssignToConstVariable pos2 ident
                otherwise -> do 
                    typesEqual [t1] [t] 
                    checkTupleAssignment pos items ts
        (TupleItemU pos) -> returnCurrentEnvT
        (TupleItemRek pos tupleIdent) -> 
            checkTupleAssignmentHelper pos tupleIdent t

checkTupleAssignment pos [] [] = returnCurrentEnvT 


    

