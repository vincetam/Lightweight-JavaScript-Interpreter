module Interp (
  isInteger,
  exec,
  resume,
  toString,
  printHeapValue
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Error
import Numeric as Numeric
import Data.Char as Char

import Ast

-- The following function interpreter binary operation
-- The following function returns an "Either value".
-- The right side represents a successful computaton.
-- The left side is an error message indicating a problem with the program.
-- The comparsion of references is not supported, always return False
-- i.e. (>, >=, <, <=, ==) does not work on Javascript Arrays and Objects

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

applyUnaOp :: Unaop -> Value -> Either ErrorMsg Value
applyUnaOp Not (BoolVal i) = Right $ BoolVal $ not i
applyUnaOp Not (IntVal 0) = Right $ BoolVal True
applyUnaOp Not _ = Right $ BoolVal False

applyOp :: Binop -> Value -> Value -> [Store] -> Either ErrorMsg Value
applyOp Plus (IntVal i) (IntVal j) h = Right $ IntVal $ i + j
applyOp Plus i j h = Right $ StrVal $ getStr i h ++ getStr j h

applyOp Minus (IntVal i) (IntVal j) h = 
  Right $ IntVal $ i - j

applyOp Minus (IntVal i) (StrVal j) h = 
  Right $ if isNumeric j 
    then IntVal (i - (read j))
    else NaNVal

applyOp Minus (StrVal i) (IntVal j) h = 
  Right $ if isNumeric i 
    then IntVal ((read i) - j)
    else NaNVal

applyOp Minus (StrVal i) (StrVal j) h = 
  Right $ if isNumeric i && isNumeric j 
    then IntVal ((read i) - (read j)) 
    else NaNVal

applyOp Minus _ _  h = Right NaNVal

applyOp Times (IntVal i) (IntVal j) h = 
  Right $ IntVal $ i * j

applyOp Times (IntVal i) (StrVal j) h = 
  Right $ if isNumeric j 
    then IntVal (i * (read j)) 
    else NaNVal

applyOp Times (StrVal i) (IntVal j) h = 
  Right $ if isNumeric i 
    then IntVal ((read i) * j) 
    else NaNVal

applyOp Times (StrVal i) (StrVal j) h = 
  Right $ if isNumeric i && isNumeric j 
    then IntVal ((read i) * (read j)) 
    else NaNVal

applyOp Times _ _ h = Right NaNVal

applyOp Divide (IntVal i) (IntVal j) h = 
  Right $ applyDivide i j

applyOp Divide (IntVal i) (StrVal j) h = 
  Right $ if isNumeric j 
    then applyDivide i (read j) 
    else NaNVal

applyOp Divide (StrVal i) (IntVal j) h = 
  Right $ if isNumeric i 
    then applyDivide (read i) j 
    else NaNVal

applyOp Divide (StrVal i) (StrVal j) h = 
  Right $ if isNumeric i && isNumeric j 
    then applyDivide (read i) (read j) 
    else NaNVal

applyOp Divide _ _ h = Right NaNVal

applyOp Gt (StrVal i) (StrVal j) h = Right $ BoolVal $ i > j
applyOp Gt i j h = applyIntOp Gt (>) i j

applyOp Ge (StrVal i) (StrVal j) h = Right $ BoolVal $ i >= j
applyOp Ge i j h = applyIntOp Ge (>=) i j

applyOp Lt (StrVal i) (StrVal j) h = Right $ BoolVal $ i < j
applyOp Lt i j h = applyIntOp Lt (<) i j

applyOp Le (StrVal i) (StrVal j) h = Right $ BoolVal $ i <= j
applyOp Le i j h = applyIntOp Le (<=) i j

applyOp Seq NullVal NullVal h = Right $ BoolVal True
applyOp Seq NaNVal NaNVal h = Right $ BoolVal True
applyOp Seq (IntVal i) (IntVal j) h = Right $ BoolVal $ i == j
applyOp Seq (BoolVal i) (BoolVal j) h = Right $ BoolVal $ i == j
applyOp Seq (StrVal i) (StrVal j) h = Right $ BoolVal $ i == j
applyOp Seq i j h = Right $ BoolVal False

applyOp Eq (StrVal i) (StrVal j) h = Right $ BoolVal $ i == j
applyOp Eq i j h = applyIntOp Eq (==) i j

applyOp Sneq i j h = case applyOp Seq i j h of
  Left msg -> Left msg
  Right (BoolVal v) -> Right $ BoolVal $ not v

applyOp Neq (StrVal i) (StrVal j) h = Right $ BoolVal $ i /= j
applyOp Neq i j h = applyIntOp Neq (/=) i j

-- Always return true if the expression contains no zero or false value
applyOp And (BoolVal False) _ h = Right $ BoolVal False
applyOp And _ (BoolVal False) h = Right $ BoolVal False
applyOp And (IntVal 0) _ h = Right $ BoolVal False
applyOp And _ (IntVal 0) h = Right $ BoolVal False
applyOp And _ _ h = Right $ BoolVal True

-- Always return true unless both side are false/zero
applyOp Or (BoolVal False) (BoolVal False) h = Right $ BoolVal False
applyOp Or (BoolVal False) (IntVal 0) h = Right $ BoolVal False
applyOp Or (IntVal 0) (BoolVal False) h = Right $ BoolVal False
applyOp Or (IntVal 0) (IntVal 0) h = Right $ BoolVal False
applyOp Or _ _ h = Right $ BoolVal True
--applyOp op t1 t2 = Left $ show op ++ " is undefined for argument type(s) of (" ++ (show t1) ++ "),(" ++ (show t2) ++ ") !"

-- toString method specified for + operator
getStr :: Value -> [Store] -> String

getStr (UdfVal) h = "undefined"

getStr (NullVal) h = "null"

getStr (NaNVal) h = "NaN"

getStr (IntVal i) h = show i

getStr (BoolVal i) h = if i then "true" else "false"

getStr (StrVal i) h = i

getStr (AryVal i) h = getStrOfAry i h

getStr (ObjVal i) h = "[object Object]"

getStr (RefVal i) (h:[]) = case Map.lookup (show i) h of
  Just refValue -> getStr refValue (h:[])
  -- reach global scope
  _ -> error "Invalid Memory Address"

getStr (RefVal i) (h:hs) = case Map.lookup (show i) h of
  Just refValue -> getStr refValue (h:hs)
  -- search parent scope
  _ -> getStr (RefVal i) hs

getStrOfAry :: [Value] -> [Store] -> String
getStrOfAry [] h = ""
getStrOfAry (x:[]) h = getStr x h
getStrOfAry (x:xs) h = getStr x h ++ "," ++ getStrOfAry xs h

getAttrOfObj :: Eq k => [(k, Value)] -> k -> Value
getAttrOfObj lst attr = 
  foldl (\acc (k,v) -> if k == attr then v else acc) NaNVal lst

getAttrOfAry :: Value -> [Variable] -> [Store] -> Value
getAttrOfAry lst idx heap@(h:hs) =
  let AryVal l = lst
      StdVar i = head idx
  in if length idx == 1
    then if isNumeric i
      then l !! (read i)
      else case evalExpr (Var (StdVar i)) (h:hs) of
        Right (IntVal index, _) -> l !! index
        _ -> error "index is not a int"
    else if isNumeric i
      then let RefVal ref = (l !! (read i)) in case Map.lookup (show ref) (last heap) of
        Just (AryVal refValue) -> getAttrOfAry (AryVal refValue) (tail idx) (h:hs)
        _ -> error "braceket notation only works on array."
      else case evalExpr (Var (StdVar i)) (h:hs) of
        Right (IntVal index, _) -> let RefVal ref = (l !! index) in case Map.lookup (show ref) (last heap) of
          Just (AryVal refValue) -> getAttrOfAry (AryVal refValue) (tail idx) (h:hs)
        _ -> error "index is not a int"

applyDivide i j =
  case j of
    0 -> NaNVal
    _ -> IntVal $ i `div` j

applyIntOp op bop NullVal NullVal = Right $ BoolVal True
applyIntOp op bop NullVal _ = Right $ BoolVal False
applyIntOp op bop _ NullVal = Right $ BoolVal False

applyIntOp op bop NaNVal NaNVal = Right $ BoolVal True
applyIntOp op bop NaNVal _ = Right $ BoolVal False
applyIntOp op bop _ NaNVal = Right $ BoolVal False

applyIntOp op bop (AryVal i) _ = Right $ BoolVal False
applyIntOp op bop _ (AryVal j) = Right $ BoolVal False

applyIntOp op bop (ObjVal i) _ = Right $ BoolVal False
applyIntOp op bop _ (ObjVal j) = Right $ BoolVal False

applyIntOp op bop (BoolVal i) (BoolVal j) = do
  arg0 <- Right $ if i then 1 else 0
  arg1 <- Right $ if j then 1 else 0
  return $ BoolVal $ bop arg0 arg1

applyIntOp op bop (IntVal i) (IntVal j) = 
  Right $ BoolVal $ bop i j

applyIntOp op bop (IntVal i) (StrVal j) = 
  Right $ BoolVal $ if isNumeric j 
    then bop i (read j) 
    else False

applyIntOp op bop (StrVal i) (IntVal j) = 
  Right $ BoolVal $ if isNumeric i 
    then bop (read i) j 
    else False

applyIntOp op bop t1 t2 = 
  Left $ show op ++ " is undefined for argument type(s) of (" ++ show t1 ++ "),(" ++ show t2 ++ ") !"

-- evalVar return the head of the variable
evalVar :: Expression -> Store -> Either ErrorMsg [Variable]
evalVar (Var var) h = do
  return $ [var]

evalVar (Bop _ var attr) h = do
  varLst <- evalVar var h
  m <- case attr of
    Var attribute -> Right $ varLst ++ [attribute]
    Bop Idx _ _ -> let Right idxLst = evalVar attr h ------------------check let right, use case
      in Right $ varLst ++ idxLst
    _ -> Left $ show attr ++ " is not a valid attribute/idx"
  return m

evalVar _ _ = do
  m <- Left "Dot notation is only supported in objects"
  return m

evalExpr :: Expression -> [Store] -> Either ErrorMsg (Value, [Store])
evalExpr (Uop op e) h = do
  (v1,h') <- evalExpr e h
  v  <- applyUnaOp op v1
  return (v, h')

-- eval dot notation
evalExpr (Bop Dot e1 e2) heap@(h:hs) = do
  (obj,_) <- evalExpr e1 (h:hs)
  m <- case e2 of

    Var (StdVar attr) -> case obj of
      RefVal ref -> case Map.lookup (show ref) (last heap) of
        Just (ObjVal refValue) -> Right (getAttrOfObj refValue attr, heap)
        Just (AryVal refValue) -> Right (IntVal (length refValue), heap)
        _ -> Left "dot notation only works on Objects"
      _ -> Left "dot notation only works on Objects"

    Bop Idx e1' e2' -> case obj of
      RefVal ref -> case Map.lookup (show ref) (last heap) of
        Just (ObjVal refValue) -> case (evalVar e2 h) of
          Right varLst -> let StdVar var = head varLst
            in let RefVal ref' =  (getAttrOfObj refValue var) in
              case Map.lookup (show ref') (last heap) of
                Just (AryVal refAry) -> Right (getAttrOfAry (AryVal refAry) (tail varLst) (h:hs), heap)
                _ -> Left "braceket notation only works on array"
          _ -> Left "invalid variable name"
        _ -> Left "dot notation only works on Objects"
      _ -> Left "dot notation only works on Objects"
    _ -> Left "dot must be followed by a variable name"

  return m

-- braceket notation
evalExpr (Bop Idx e1 e2) heap@(h:hs) = do
  (ary,_) <- evalExpr e1 heap
  m <- case e2 of
    Var (StdVar idx) -> case ary of

      RefVal ref -> case Map.lookup (show ref) (last heap) of
        Just (AryVal refValue) -> if isNumeric idx
          then Right (refValue !! (read idx), heap)
          -- if the index is a expression, eval further
          else case evalExpr e2 (h:hs) of
            Right (IntVal i,_) -> Right (refValue !! i, heap)
            _ -> Left "index must be a int"
        _ -> Left "Memory Address corrupted."
      _ -> Left "bracket notation only works on Arrays"

    _ -> Left "dot must be followed by a variable name"
  return m

evalExpr (Bop o e1 e2) h = do
  (v1,h1) <- evalExpr e1 h
  (v2,h') <- evalExpr e2 h1
  v  <- applyOp o v1 v2 h'
  return (v, h')

evalExpr (Val v) h = do
  return (v, h)

evalExpr (Var (StdVar var)) heap = do
  m <- case lookupVar var heap of
    Just v  -> Right (v, heap)
    Nothing -> Right (UdfVal, heap)
  return m

evalExpr (Func fid exprs) heap@(h:hs) = do
  (r,h') <- case lookupVar fid heap of
    Just (FuncVal idents s) -> case makeStoreWithVars idents exprs (Map.empty:heap) of
      Right h'' -> evalStmt s h''
      Left msg -> Left msg
    _ -> Right (UdfVal, Map.empty:heap)
  return (r, (tail h'))


evalExpr expr h = Left $ "Invalid expression :" ++ show expr

-- look up a variable from local scope to parent scope
lookupVar var (h:[]) = Map.lookup var h
lookupVar var (h:hs) = case Map.lookup var h of
  Just v -> Just v
  Nothing -> lookupVar var hs

makeStoreWithVars :: [Identifier] -> [Expression] -> [Store] -> Either ErrorMsg [Store]
makeStoreWithVars [] _ h = Right h
makeStoreWithVars _ [] h = Right h
makeStoreWithVars (ident:idents) (expr:exprs) h = do
  (v,_) <- evalExpr expr (tail h)
  (v,h') <- evalStmt (Assign (Var (StdVar ident)) (Val v)) h
  h'' <- makeStoreWithVars idents exprs h'
  return h''

storeArray :: String -> [Value] -> [Store] -> [Store]
storeArray variable ary m = let (fmAry, mem) = (storeArray' variable ary (last m)) in
  case Map.lookup _ref_mem_loc mem of
    Just (RefVal memIdx) -> case length m of
      1 -> [Map.insert _ref_mem_loc (RefVal (memIdx+1)) 
        (Map.insert (show memIdx) (AryVal fmAry) (Map.insert variable (RefVal memIdx) mem))]
      _ -> (Map.insert variable (RefVal memIdx) (head m)):(tail (init m ++ 
        [Map.insert _ref_mem_loc (RefVal (memIdx+1)) (Map.insert (show memIdx) (AryVal fmAry) mem)]))
    _ -> error "Memory Address corrupted."

storeArray' :: String -> [Value] -> Store -> ([Value], Store)
storeArray' variable [] m = ([],m)
storeArray' variable (x:xs) m = case x of
  AryVal ary -> let (fmAry, mem) = storeArray' variable xs m in
    case Map.lookup _ref_mem_loc mem of
      Just (RefVal memIdx) -> (((RefVal memIdx):fmAry), (storeArray'' (show memIdx) ary 
        (Map.insert _ref_mem_loc (RefVal (memIdx+1)) mem)))
      _ -> error "Memory Address corrupted."
  ObjVal obj -> let (fmAry, mem) = storeArray' variable xs m in
    case Map.lookup _ref_mem_loc mem of
      Just (RefVal memIdx) -> (((RefVal memIdx):fmAry), (storeObject'' (show memIdx) obj 
        (Map.insert _ref_mem_loc (RefVal (memIdx+1)) mem)))
      _ -> error "Memory Address corrupted."
  _ -> let (fmAry, mem) = storeArray' variable xs m in ((x:fmAry), mem)

storeArray'' :: String -> [Value] -> Store -> Store
storeArray'' variable ary m = 
  let (fmAry, mem) = (storeArray' variable ary m) in Map.insert variable (AryVal fmAry) mem

storeObject :: String -> [(String, Value)] -> [Store] -> [Store]
storeObject variable obj m = let (fmObj, mem) = (storeObject' variable obj (last m)) in
  case Map.lookup _ref_mem_loc mem of
    Just (RefVal memIdx) -> case length m of
      1 -> [Map.insert _ref_mem_loc (RefVal (memIdx+1)) 
        (Map.insert (show memIdx) (ObjVal fmObj) (Map.insert variable (RefVal memIdx) mem))]
      _ -> (Map.insert variable (RefVal memIdx) (head m)):(tail (init m ++ 
        [Map.insert _ref_mem_loc (RefVal (memIdx+1)) (Map.insert (show memIdx) (ObjVal fmObj) mem)]))
    _ -> error "Memory Address corrupted."

storeObject' :: String -> [(String, Value)] -> Store -> ([(String, Value)], Store)
storeObject' variable [] m = ([],m)
storeObject' variable (x:xs) m = case x of
  (k, AryVal ary) -> let (fmAry, mem) = storeObject' variable xs m in
    case Map.lookup _ref_mem_loc mem of
      Just (RefVal memIdx) -> (((k, RefVal memIdx):fmAry), (storeArray'' (show memIdx) ary 
        (Map.insert _ref_mem_loc (RefVal (memIdx+1)) mem)))
      _ -> error "Memory Address corrupted."
  (k, ObjVal obj) -> let (fmObj, mem) = storeObject' variable xs m in
    case Map.lookup _ref_mem_loc mem of
      Just (RefVal memIdx) -> (((k, RefVal memIdx):fmObj), (storeObject'' (show memIdx) obj 
        (Map.insert _ref_mem_loc (RefVal (memIdx+1)) mem)))
      _ -> error "Memory Address corrupted."
  _ -> let (fmObj, mem) = storeObject' variable xs m in ((x:fmObj), mem)

storeObject'' :: String -> [(String, Value)] -> Store -> Store
storeObject'' variable obj m = 
  let (fmObj, mem) = (storeObject' variable obj m) in Map.insert variable (ObjVal fmObj) mem

-- LEFT msg                             - dot/bracklet notation on a non-existing object
-- RIGHT (string, Nothing)              - standard assignment
-- RIGHT (identifier, index/attribute)  - assignment using dot/braceket notation
getRef :: Expression -> [Store] -> Either ErrorMsg (Identifier, Maybe Value)
getRef (Var (StdVar variable)) heap = Right (variable, Nothing)

-- eval dot notation
getRef (Bop Dot e1 e2) heap@(h:hs) = do
  (obj,_) <- evalExpr e1 heap
  m <- case e2 of

    Var (StdVar attr) -> case obj of
      RefVal ref -> Right (show ref, Just (StrVal attr))

    Bop Idx e1' e2' -> case evalExpr (Bop Dot e1 e1') heap of
      Right (RefVal ref, _) -> case e2' of
        (Var (StdVar idx)) -> Right (show ref, Just (IntVal (read idx)))
        _ -> Left "index must be a int"
      Right val -> Left $ "braceket notation only works on array "
      Left msg -> Left $ msg ++ show (Bop Idx e1' e2')

  return m

-- braceket notation
getRef (Bop Idx e1 e2) heap@(h:hs) = do
  (ary,_) <- evalExpr e1 heap
  m <- case e2 of
    Var (StdVar idx) -> case ary of

      RefVal ref -> if isNumeric idx
        then Right (show ref, Just (IntVal (read idx)))
        else case evalExpr e2 heap of
          Right (IntVal i,_) -> Right (show ref, Just (IntVal i))
          _ -> Left "index must be a int"

    _ -> Left "Braceket notation only works on int index"
  return m

getRef varExpr heap = Left "Assignment on a non-existing object"

updateByIdx variable idx val heap = case Map.lookup variable (last heap) of
  Just (AryVal array) -> case val of

    AryVal ary -> let heap' = storeArray variable ary heap in
      case Map.lookup variable (head heap') of
        Just (RefVal ref) -> Right (UdfVal, init heap' ++ 
          [(Map.insert variable (AryVal (updateByIdx' array idx (RefVal ref))) (last heap'))])
        _ -> Left "Memory corrupted."

    ObjVal obj -> let heap' = storeObject variable obj heap in
      case Map.lookup variable (head heap') of
        Just (RefVal ref) -> Right (UdfVal, init heap' ++ 
          [(Map.insert variable (AryVal (updateByIdx' array idx (RefVal ref))) (last heap'))])
        _ -> Left "Memory corrupted."

    _ -> Right (UdfVal, init heap ++ 
      [(Map.insert variable (AryVal (updateByIdx' array idx val)) (last heap))])

  _ -> Left "Braceket notation only supported on array"

updateByIdx' (x:xs) idx val
  | idx == 0 = val:xs
  | otherwise = x:updateByIdx' xs (idx - 1) val

updateByAttr variable attr val heap = case Map.lookup variable (last heap) of
  Just (ObjVal object) -> case val of

    AryVal ary -> let heap' = storeArray variable ary heap in
      case Map.lookup variable (head heap') of
        Just (RefVal ref) -> Right (UdfVal, init heap' ++ 
          [(Map.insert variable (ObjVal (updateByAttr' object attr (RefVal ref))) (last heap'))])
        _ -> Left "Memory corrupted."

    ObjVal obj -> let heap' = storeObject variable obj heap in
      case Map.lookup variable (head heap') of
        Just (RefVal ref) -> Right (UdfVal, init heap' ++ 
          [(Map.insert variable (ObjVal (updateByAttr' object attr (RefVal ref))) (last heap'))])
        _ -> Left "Memory corrupted."

    _ -> Right (UdfVal, init heap ++ 
      [(Map.insert variable (ObjVal (updateByAttr' object attr val)) (last heap))])

  _ -> Left "Dot notation only supported on array"

updateByAttr' [] attr val = [(attr, val)]
updateByAttr' (x@(k, v):xs) attr val
  | k == attr = (k, val):xs
  | otherwise = x:updateByAttr' xs attr val

evalStmt :: Statement -> [Store] -> Either ErrorMsg (Value, [Store])

evalStmt (Assign varExpr e) heap@(h:hs) = do
  (val,heap'@(h':hs')) <- evalExpr e (h:hs)
  case getRef varExpr heap of

    Right (variable, Nothing) -> case val of
      AryVal ary -> Right (UdfVal, storeArray variable ary heap')
      ObjVal obj -> Right (UdfVal, storeObject variable obj heap')
      _ -> Right (UdfVal, (Map.insert variable val h'):hs')

    Right (variable, Just (IntVal idx)) -> updateByIdx variable idx val heap'

    Right (variable, Just (StrVal attr)) -> updateByAttr variable attr val heap'

    Left msg -> Left msg

evalStmt (Return e) h = do
  (v,h') <- evalExpr e h
  return (v,h')

evalStmt (Sequence s1 s2) h = do
  (v,h1)  <- evalStmt s1 h
  (v',h') <- case v of
    UdfVal -> evalStmt s2 h1
    _ -> Right (v,h1)
  return (v',h')

evalStmt (If s1 s2) (h:hs) = do
  (cond,h') <- evalExpr s1 (h:hs)
  m <- case cond of
    BoolVal True -> evalStmt s2 h'
    BoolVal False -> Right (UdfVal, h')
    IntVal errorVal -> Left $ "Non-boolean value '" ++ show errorVal ++ "' used as a conditional"
  return m

evalStmt (IfElse e s1 s2) (h:hs) = do
  (cond,h') <- evalExpr e (h:hs)
  m <- case cond of
    BoolVal True -> evalStmt s1 h'
    BoolVal False -> evalStmt s2 h'
    IntVal errorVal -> Left $ "Non-boolean value '" ++ show errorVal ++ "' used as a conditional"
  return m

evalStmt (While e s) (h:hs) = do
  (cond,h') <- evalExpr e (h:hs)
  m <- case cond of
    BoolVal True -> evalStmt (Sequence s (While e s)) h'
    BoolVal False -> Right (UdfVal, h')
    IntVal errorVal -> Left $ "Non-boolean value '" ++ show errorVal ++ "' used as a conditional"
  return m

evalStmt (DoWhile s e) (h:hs) = do
  (cond,h') <- evalExpr e (h:hs)
  evalStmt (Sequence s (While e s)) h'

evalStmt (For f0 f1 f2 s) h = do
  evalStmt (Sequence f0 (While f1 (Sequence s f2))) h

evalStmt (Log e) (h:[]) = do
  (v,heap'@(h':[])) <- evalExpr e (h:[])
  str <- Right $ toString v heap'
  m <- case Map.lookup _ostream_mem_loc h' of
    Just (StrVal output) -> Right (UdfVal, ((Map.insert _ostream_mem_loc (StrVal (output ++ str ++ "\n")) h'):[]))
    _ -> Left $ "Memory location for screen output is corrupted"
  return m

evalStmt (Log e) heap@(h:hs) = do
  (v,heap'@(h':hs')) <- evalExpr e (h:hs)
  str <- Right $ toString v heap'
  m <- case Map.lookup _ostream_mem_loc (last heap') of
    Just (StrVal output) -> Right (UdfVal, (init heap' ++ 
      [(Map.insert _ostream_mem_loc (StrVal (output ++ str ++ "\n")) (last heap'))]))
    _ -> Left $ "Memory location for screen output is corrupted"
  return m

evalStmt (Skip) h = do
  return (UdfVal, h)

evalStmt (FuncDef fid idents s) (h:hs) = do
  h' <- Right $ Map.insert fid (FuncVal idents s) h
  return (UdfVal, h':hs)

evalStmt (FuncApp e) h = do
  (v,h') <- evalExpr e h
  return (UdfVal, h')

evalStmt (Block s) (h:hs) = do
  (v, (h':hs')) <- evalStmt s ((Map.insert _ref_mem_loc (RefVal _ref_mem_idx) Map.empty):(h:hs))
  return (UdfVal, hs')

evalStmt stmt h = Left $ "Invalid statement :" ++ show stmt


isValidAssign :: Expression -> [Store] -> Bool
isValidAssign (Bop Idx e1 e2) h = True -------------------------------fix here

isValidAssign (Bop Dot e1 e2) h =
  case e2 of
    Bop Idx _ _ -> True ----------------------------------------------fix here
    (Var _) -> case evalExpr e1 h of
      Right (ObjVal _, _) -> True
      _ -> False

replaceByAttr :: Identifier -> [Variable] -> Value -> [Store] -> Value
replaceByAttr variable varLst val (h:hs) =
  case evalExpr (Var (StdVar variable)) (h:hs) of
    Right (ObjVal obj, _) -> ObjVal $ replaceByAttr' obj (tail varLst) val (h:hs)
    Right (AryVal ary, _) -> AryVal $ replaceByIdx' ary (tail varLst) val (h:hs)
    _ -> NaNVal

-- insert new attr if not found
replaceByAttr' :: [(Identifier, Value)] -> [Variable] -> Value -> [Store] -> [(Identifier, Value)]
replaceByAttr' [] varLst val h =
  let StdVar attr = head varLst
  in [(attr, val)]

-- since varLst is checked by isValidAssign, no further error checking
replaceByAttr' (x@(k,v):xs) varLst val h =
  let StdVar attr = head varLst
  in if attr == k
    then if length varLst == 1
      then (k, val) : xs
      else
        case v of
          ObjVal obj -> (k, ObjVal (replaceByAttr' obj (tail varLst) val h)) : xs
          AryVal ary -> (k, AryVal (replaceByIdx' ary (tail varLst) val h)) : xs
          _ -> error "invalid structure"
    else x : replaceByAttr' xs varLst val h

replaceElemByIdx idx val (x:xs)
  | idx == 0 = val:xs
  | otherwise = x:replaceElemByIdx (idx - 1) val xs

replaceByIdx' ary varLst val h =
  let StdVar idx = head varLst
  in if isNumeric idx
    then let index = read idx
      in if length varLst == 1
        then replaceElemByIdx index val ary
        else case ary !! index of
          AryVal elem -> replaceElemByIdx index (AryVal (replaceByIdx' elem (tail varLst) val h)) ary
          ObjVal elem -> replaceElemByIdx index (ObjVal (replaceByAttr' elem (tail varLst) val h)) ary
    else case evalExpr (Var (StdVar idx)) h of
      Right (IntVal index, _) -> if length varLst == 1
        then replaceElemByIdx index val ary
        else case ary !! index of
          AryVal elem -> replaceElemByIdx index (AryVal (replaceByIdx' elem (tail varLst) val h)) ary
          ObjVal elem -> replaceElemByIdx index (ObjVal (replaceByAttr' elem (tail varLst) val h)) ary
      Left _ -> error "TBD"

toString :: Value -> [Store] -> String
toString (UdfVal) h = "undefined"
toString (NullVal) h = "null"
toString (NaNVal) h = "NaN"
toString (IntVal i) h = show i
toString (BoolVal i) h = if i then "true" else "false"
toString (StrVal i) h = i
toString (AryVal i) h = "[" ++ aryToStr i h ++ "]"
toString (ObjVal i) h = "Object {" ++ objToStr i h ++ "}"

toString (FuncVal [] s) h = "Function(): " ++ show s
toString (FuncVal idents s) h = "Function(" 
  ++ tail (foldl (\acc elem -> acc ++ "," ++ elem) "" idents) ++ "): " ++ show s

toString (RefVal i) (h:[]) = case Map.lookup (show i) h of
  Just refValue -> toString refValue (h:[])
  -- reach global scope
  _ -> error "Invalid Memory Address"
toString (RefVal i) (h:hs) = case Map.lookup (show i) h of
  Just refValue -> toString refValue (h:hs)
  -- search parent scope
  _ -> toString (RefVal i) hs

-- insert \" on string inside Array and Object
aryToStr :: [Value] -> [Store] -> String
aryToStr [] h = ""
aryToStr ((StrVal v):[]) h = "\"" ++ v ++ "\""
aryToStr (v:[]) h = toString v h
aryToStr ((StrVal v):vs) h = "\"" ++ v ++ "\"" ++ ", " ++ aryToStr vs h
aryToStr (v:vs) h = toString v h ++ ", " ++ aryToStr vs h

objToStr :: [(String, Value)] -> [Store] -> String
objToStr [] h = ""
objToStr ((k,(StrVal v)):[]) h = k ++ ": " ++ "\"" ++ v ++ "\""
objToStr ((k,v):[]) h = k ++ ": " ++ toString v h
objToStr ((k,(StrVal v)):rst) h = k ++ ": "  ++ "\"" ++ v ++ "\"" ++  ", " ++ objToStr rst h
objToStr ((k,v):rst) h = k ++ ": "  ++ toString v h ++  ", " ++ objToStr rst h

printHeapValue (AryVal i) = "[" ++ aryToStr' i ++ "]"
printHeapValue (ObjVal i) = "Object {" ++ objToStr' i ++ "}"
printHeapValue (RefVal i) = "0x" ++ (Numeric.showIntAtBase 16 Char.intToDigit i "")
printHeapValue i = toString i [Map.empty]

aryToStr' :: [Value] -> String
aryToStr' [] = ""
aryToStr' ((StrVal v):[]) = "\"" ++ v ++ "\""
aryToStr' (v:[]) = printHeapValue v
aryToStr' ((StrVal v):vs) = "\"" ++ v ++ "\"" ++ ", " ++ aryToStr' vs
aryToStr' (v:vs) = printHeapValue v ++ ", " ++ aryToStr' vs

objToStr' :: [(String, Value)] -> String
objToStr' [] = ""
objToStr' ((k,(StrVal v)):[]) = k ++ ": " ++ "\"" ++ v ++ "\""
objToStr' ((k,v):[]) = k ++ ": " ++ printHeapValue v
objToStr' ((k,(StrVal v)):rst) = k ++ ": "  ++ "\"" ++ v ++ "\"" ++  ", " ++ objToStr' rst
objToStr' ((k,v):rst) = k ++ ": "  ++ printHeapValue v ++  ", " ++ objToStr' rst

-- Evaluates a program with an initially empty state
exec :: Statement -> Either ErrorMsg (Value, [Store])
-- assume the screen is empty
exec prog = evalStmt prog [(Map.insert _ref_mem_loc (RefVal _ref_mem_idx)
  (Map.insert _ostream_mem_loc (StrVal "") Map.empty))]

-- resume the last store, and empty the screen
resume prog heap = evalStmt prog $ (Map.insert _ostream_mem_loc (StrVal "") (head heap)):(tail heap)

