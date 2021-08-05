module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
-- procedure envrionment (maps procedure names to procedure bodies)
type PEnv = H.HashMap String Stmt

-- result of executing a statement 
type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String    -- exception
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
        --  | AbsExp Exp
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
          | ExpStmt Exp
          | AbsStmt Exp
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal (op x y)
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
    case H.lookup s env of
        Just s -> s
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env = 
    case (H.lookup op intOps) of
        (Just f) -> 
            let v1 = eval e1 env
                v2 = eval e2 env
            in case (op,v2) of
                ("/", IntVal 0) -> ExnVal "Division by 0"
                _               -> liftIntOp f v1 v2
        _ -> ExnVal "No matching operator"

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op boolOps
     in liftBoolOp f v1 v2

eval (CompOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op compOps
     in liftCompOp f v1 v2

--- ### If Expressions

eval (IfExp e1 e2 e3) env = 
    let v1 = eval e1 env
     in case v1 of
         BoolVal True -> eval e2 env
         BoolVal False -> eval e3 env
         _            -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application
eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    let v1 = eval e1 env
    in case v1 of
        (CloVal xs body clenv) ->
            let p = map(\x -> eval x env) args
                env_new = H.fromList (zip xs p)
             in eval body (H.union env_new clenv)
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions
eval (LetExp pairs body) env = 
    let p = map (\(s,e) -> (s, eval e env)) pairs
        new_env = H.fromList p
     in eval body (H.union new_env env)
    
-- ### Abs Expressions
-- eval (AbsExp e1) env = 
--     let v1 = eval e1 env
--      in case v1 of
--          IntVal x1 -> case x1 >= 0 of
--              True  -> v1
--              False -> eval (IntOpExp "-" (IntExp 0) (IntExp x1)) env
--          _         -> ExnVal "The argument must be an integer."

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = (val, penv, env1)
    where val = ""
          env1 = H.insert var (eval e env) env
          

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (s1:ss)) penv env = (val, penv2, env2)
    where (v1, penv1, env1) = exec (s1) penv env
          (v2, penv2, env2) = exec (SeqStmt ss) penv1 env1
          val = v1 ++ v2

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = 
    let v1 = eval e1 env
     in case v1 of
         BoolVal True -> exec s1 penv env
         BoolVal False -> exec s2 penv env
         _            -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("",penv1,env)
    where penv1 = H.insert name p penv

exec (CallStmt name args) penv env = 
    case funEst of
        Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)
        Just (ProcedureStmt name v body) -> 
            let arg = map (\x -> eval x env) args
                new_env = H.union (H.fromList (zip v arg)) env
             in exec body penv new_env
        where funEst = H.lookup name penv
                  
--- ### Abs Statements
exec (AbsStmt e1) penv env = 
    let v1 = eval e1 env
     in case v1 of
         IntVal x1 -> case x1 >= 0 of
             True  -> (show v1, penv, env)
             False -> (show $ eval (IntOpExp "-" (IntExp 0) (IntExp x1)) env, penv, env)
         _         -> ("exn: The argument must be an integer.", penv, env)

--- ### Expression Statements

exec (ExpStmt e) penv env = (val, penv, env)
    where val = show $ eval e env