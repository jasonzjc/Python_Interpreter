module Lib where

import Data.Char
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

--- Data Types
--- ----------

--- ### Environments and Results

type Env = H.HashMap String Val

-- procedure envrionment (maps procedure names to procedure bodies)
type PEnv = H.HashMap String Stmt

-- result of executing a statement
type Result = (String, PEnv, Env)

--- ### Values

data Val
  = IntVal Int
  | DoubleVal Double
  | BoolVal Bool
  | NoneVal
  | StrVal String
  | CloVal [String] Exp Env
  | ExnVal String -- exception
  deriving (Eq)

instance Show Val where
  show (IntVal i) = show i
  show (DoubleVal f) = show f
  show (BoolVal i) = show i
  show NoneVal = "None"
  show (StrVal s) = show s
  show (CloVal xs body env) =
    "<" ++ show xs ++ ", "
      ++ show body
      ++ ", "
      ++ show env
      ++ ">"
  show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp
  = IntExp Int
  | DoubleExp Double
  | BoolExp Bool
  | NoneExp
  | StrExp String
  | FunExp [String] Exp
  | LetExp [(String, Exp)] Exp
  | AppExp Exp [Exp]
  | IfExp Exp Exp Exp
  | NumOpExp String Exp Exp
  | NotExp Exp
  | BoolOpExp String Exp Exp
  | CompOpExp String Exp Exp
  | VarExp String
  deriving (Show, Eq)

--- ### Statements

data Stmt
  = SetStmt String Exp
  | PrintStmt Exp
  | QuitStmt
  | IfStmt Exp Stmt Stmt
  | ProcedureStmt String [String] Stmt
  | CallStmt String [Exp]
  | SeqStmt [Stmt]
  | ExpStmt Exp
  | AbsStmt Exp
  | BoolStmt Exp
  | CharStmt Exp
  | EvalStmt Exp
  | RoundStmt Exp Exp
  deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps =
  H.fromList
    [ ("+", (+)),
      ("-", (-)),
      ("*", (*)),
      ("/", (div))
    ]

doubleOps :: H.HashMap String (Double -> Double -> Double)
doubleOps =
  H.fromList
    [ ("+", (+)),
      ("-", (-)),
      ("*", (*)),
      ("/", (/))
    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps =
  H.fromList
    [ ("and", (&&)),
      ("or", (||))
    ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps =
  H.fromList
    [ ("<", (<)),
      (">", (>)),
      ("<=", (<=)),
      (">=", (>=)),
      ("/=", (/=)),
      ("==", (==))
    ]

compDoubleOps :: H.HashMap String (Double -> Double -> Bool)
compDoubleOps =
  H.fromList
    [ ("<", (<)),
      (">", (>)),
      ("<=", (<=)),
      (">=", (>=)),
      ("/=", (/=)),
      ("==", (==))
    ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal (op x y)
liftIntOp _ _ _ = ExnVal "Cannot lift"

-- liftIntOp :: (Num a) => (a -> a -> a) -> Val -> Val -> Val
-- liftIntOp op (IntVal x) (IntVal y) = IntVal (op x y)
-- liftIntOp op (DoubleVal x) (DoubleVal y) = DoubleVal (op x y)
-- liftIntOp _ _ _ = ExnVal "Cannot lift"

liftDoubleOp :: (Double -> Double -> Double) -> Val -> Val -> Val
liftDoubleOp op (DoubleVal x) (DoubleVal y) = DoubleVal (op x y)
liftDoubleOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

liftDoubleCompOp :: (Double -> Double -> Bool) -> Val -> Val -> Val
liftDoubleCompOp op (DoubleVal x) (DoubleVal y) = BoolVal $ op x y
liftDoubleCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val
--- ### Constants
eval (IntExp i) _ = IntVal i
eval (DoubleExp f) _ = DoubleVal f
eval (BoolExp i) _ = BoolVal i
eval NoneExp _ = NoneVal
eval (StrExp s) _ = StrVal s
--- ### Variables

eval (VarExp s) env =
  case H.lookup s env of
    Just s -> s
    Nothing -> ExnVal "No match in env"
--- ### Arithmetic

<<<<<<< HEAD
eval (NumOpExp op e1 e2) env = 
    case ((H.lookup op intOps),(H.lookup op doubleOps)) of
        (Just f1, Just f2) -> 
            let v1 = eval e1 env
                v2 = eval e2 env
            in case (op,v1,v2) of
                ("/", IntVal _, IntVal 0) -> ExnVal "Division by 0"
                (_, IntVal _, IntVal _) -> liftIntOp f1 v1 v2
                (_, DoubleVal _, DoubleVal _) -> liftDoubleOp f2 v1 v2
                (_, ExnVal "Division by 0", _) -> ExnVal "Division by 0"
                (_, _, ExnVal "Division by 0") -> ExnVal "Division by 0"
                _ -> ExnVal "Cannot lift"
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
        (Just f1, Just f2) = (H.lookup op compOps, H.lookup op compDoubleOps)
     in case (v1, v2) of
         (IntVal _, IntVal _) -> liftCompOp f1 v1 v2
         (DoubleVal _, DoubleVal _) -> liftDoubleCompOp f2 v1 v2
         _ -> ExnVal "Cannot lift"

=======
eval (NumOpExp op e1 e2) env =
  case ((H.lookup op intOps), (H.lookup op doubleOps)) of
    (Just f1, Just f2) ->
      let v1 = eval e1 env
          v2 = eval e2 env
       in case (op, v1, v2) of
            ("/", IntVal _, IntVal 0) -> ExnVal "Division by 0"
            (_, IntVal _, IntVal _) -> liftIntOp f1 v1 v2
            (_, DoubleVal _, DoubleVal _) -> liftDoubleOp f2 v1 v2
    _ -> ExnVal "No matching operator"
-- eval (DoubleOpExp op e1 e2) env =
--     case (H.lookup op doubleOps) of
--         (Just f) ->
--             let v1 = eval e1 env
--                 v2 = eval e2 env
--             in case (op,v2) of
--                 ("/", DoubleVal 0) -> ExnVal "Division by 0"
--                 _               -> liftDoubleOp f v1 v2
--         _ -> ExnVal "No matching operator"

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op boolOps
   in liftBoolOp f v1 v2
eval (CompOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      (Just f1, Just f2) = (H.lookup op compOps, H.lookup op compDoubleOps)
   in case (v1, v2) of
        (IntVal _, IntVal _) -> liftCompOp f1 v1 v2
        (DoubleVal _, DoubleVal _) -> liftDoubleCompOp f2 v1 v2
>>>>>>> d4c9b65d9978d350df571b00a736a610d50a99ab
-- ### Not expression
eval (NotExp e1) env =
  let v1 = eval e1 env
   in case v1 of
        BoolVal x -> BoolVal $ not x
        _ -> ExnVal "Argument of 'not' is not a Bool"
--- ### If Expressions

eval (IfExp e1 e2 e3) env =
  let v1 = eval e1 env
   in case v1 of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _ -> ExnVal "Condition is not a Bool"
--- ### Functions and Function Application
eval (FunExp params body) env = CloVal params body env
eval (AppExp e1 args) env =
  let v1 = eval e1 env
   in case v1 of
        (CloVal xs body clenv) ->
          let p = map (\x -> eval x env) args
              env_new = H.fromList (zip xs p)
           in eval body (H.union env_new clenv)
        _ -> ExnVal "Apply to non-closure"
--- ### Let Expressions
<<<<<<< HEAD
eval (LetExp pairs body) env = 
    let p = map (\(s,e) -> (s, eval e env)) pairs
        new_env = H.fromList p
     in eval body (H.union new_env env)
    
=======
eval (LetExp pairs body) env =
  let p = map (\(s, e) -> (s, eval e env)) pairs
      new_env = H.fromList p
   in eval body (H.union new_env env)

-- ### Abs Expressions
-- eval (AbsExp e1) env =
--     let v1 = eval e1 env
--      in case v1 of
--          IntVal x1 -> case x1 >= 0 of
--              True  -> v1
--              False -> eval (NumOpExp "-" (IntExp 0) (IntExp x1)) env
--          _         -> ExnVal "The argument must be an integer."

>>>>>>> d4c9b65d9978d350df571b00a736a610d50a99ab
--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
  where
    val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = (val, penv, env1)
  where
    val = ""
    env1 = H.insert var (eval e env) env

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (s1 : ss)) penv env = (val, penv2, env2)
  where
    (v1, penv1, env1) = exec (s1) penv env
    (v2, penv2, env2) = exec (SeqStmt ss) penv1 env1
    val = v1 ++ v2

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env =
  let v1 = eval e1 env
   in case v1 of
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _ -> ("exn: Condition is not a Bool", penv, env)
--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", penv1, env)
  where
    penv1 = H.insert name p penv
exec (CallStmt name args) penv env =
  case funEst of
    Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)
    Just (ProcedureStmt name v body) ->
      let arg = map (\x -> eval x env) args
          new_env = H.union (H.fromList (zip v arg)) env
       in exec body penv new_env
  where
    funEst = H.lookup name penv

--- ### Abs Statements

exec (AbsStmt e1) penv env =
  let v1 = eval e1 env
   in case v1 of
        IntVal x1 -> case x1 >= 0 of
          True -> (show v1, penv, env)
          False -> (show $ eval (NumOpExp "-" (IntExp 0) (IntExp x1)) env, penv, env)
        DoubleVal x1 -> case x1 >= 0 of
          True -> (show v1, penv, env)
          False -> (show $ eval (NumOpExp "-" (DoubleExp 0) (DoubleExp x1)) env, penv, env)
        _ -> ("exn: The argument must be an integer.", penv, env)
--- ### Bool Statements

exec (BoolStmt e1) penv env =
  let v1 = eval e1 env
   in case v1 of
        IntVal x1 -> (show $ x1 /= 0, penv, env)
        DoubleVal x1 -> (show $ x1 /= 0.0, penv, env)
        BoolVal x1 -> (show $ x1, penv, env)
        StrVal [] -> (show False, penv, env)
        StrVal _ -> (show True, penv, env)
--- ### Char Statements

exec (CharStmt e1) penv env =
  let v1 = eval e1 env
   in case v1 of
        IntVal x -> (show $ c, penv, env)
          where
            c = chr x
        _ -> ("exn: The argument is not defined or out or range.", penv, env)
--- ### Eval Statements

exec (EvalStmt e) penv env = (val, penv, env)
  where
    val = show $ eval e env

--- ### Expression Statements

exec (ExpStmt e) penv env = (val, penv, env)
  where
    val = show $ eval e env

--- ### Round Statements
exec (RoundStmt e1 e2) penv env =
  let v1 = eval e1 env
      v2 = eval e2 env
   in case (v1, v2) of
        (IntVal i1, IntVal i2) -> (show $ i1, penv, env)
        -- where z1 = rd fromIntegral(i1) i2
        (DoubleVal d1, IntVal i2) -> (show $ y1, penv, env)
          where
            y1 = rd d1 i2
        _ -> ("exn: The argument is not defined or out or range.", penv, env)

rd :: (RealFrac a, Integral b, Fractional p) => a -> b -> p
rd x n =
  let flag = x * (10 ^ n) - fromIntegral (floor (x * (10 ^ n)))
   in case flag > 0.5 of
        True -> (fromIntegral (ceiling (x * (10 ^ n)))) / (10 ^ n)
        False -> (fromIntegral (floor (x * (10 ^ n)))) / (10 ^ n)