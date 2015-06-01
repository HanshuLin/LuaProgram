module Executer (
  excuteFile
) where
import SymTab
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import System.Environment
import Data.IORef
import System.Random
import Data.Maybe

adrs = mkStdGen 30
output = Map.insert "_#OUTPUT" Map.empty Map.empty

-- ==========RESERVED FUNCTION==========
globalEnv =  Map.fromList [("_outer",VReg "_ENV"),
                           ("_arg",VReg "_ENV")]

reservedTable =  Map.fromList [("if", VFunc "cond" (Val (VFunc "t" (Val (VFunc "f" (Funcall
                               (Funcall (Val (VArg "cond")) (Val (VArg "t"))) (Val (VArg "f")))))))),
                               ("while", VFunc "cond" (Val (VFunc "t" (Val (VFunc "f" (Funcall
                                   (Funcall (Val (VArg "cond")) (Val (VArg "t"))) (Val (VArg "f")))))))),
                               ("getValue", VFunc "_var" (Val (VFunc "_local" (Funcall (Val (VFunc "_arg" (Funcall (Val (VFunc "_outer" (Funcall (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "if"))) (Opraw (Val (VArg "_local")) Eq (Val (VReg "_ENV")))) (Val (VFunc "" (Rget (Val (VReg "_ENV")) (Val (VArg "_var")))))) (Val (VFunc "" (Funcall (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "if"))) (Opraw (Rget (Val (VArg "_local")) (Val (VArg "_var"))) Nq (Val VNil))) (Val (VFunc "" (Rget (Val (VArg "_local")) (Val (VArg "_var")))))) (Val (VFunc "" (Funcall (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "if"))) (Opraw (Rget (Val (VArg "_arg")) (Val (VArg "_var"))) Nq (Val VNil))) (Val (VFunc "" (Rget (Val (VArg "_arg")) (Val (VArg "_var")))))) (Val (VFunc "" (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "getValue"))) (Val (VArg "_var"))) (Val (VArg "_outer")))))) (Val VNil))))) (Val VNil))))) (Val VNil)))) (Rget (Val (VArg "_local")) (Val (VStr "_outer")))))) (Rget (Val (VArg "_local")) (Val (VStr "_arg"))))))),
                               ("setValue", VFunc "_var" (Val (VFunc "_value" (Val (VFunc "_local" (Funcall (Val (VFunc "_arg" (Funcall (Val (VFunc "_outer" (Funcall (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "if"))) (Opraw (Val (VArg "_local")) Eq (Val (VReg "_ENV")))) (Val (VFunc "" (Rset (Val (VReg "_ENV")) (Val (VArg "_var")) (Val (VArg "_value")))))) (Val (VFunc "" (Funcall (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "if"))) (Opraw (Rget (Val (VArg "_local")) (Val (VArg "_var"))) Nq (Val VNil))) (Val (VFunc "" (Rset (Val (VArg "_local")) (Val (VArg "_var")) (Val (VArg "_value")))))) (Val (VFunc "" (Funcall (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "if"))) (Opraw (Rget (Val (VArg "_arg")) (Val (VArg "_var"))) Nq (Val VNil))) (Val (VFunc "" (Rset (Val (VArg "_local")) (Val (VArg "_var")) (Val (VArg "_value")))))) (Val (VFunc "" (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "setValue"))) (Val (VArg "_var"))) (Val (VArg "_value"))) (Val (VArg "_outer")))))) (Val VNil))))) (Val VNil))))) (Val VNil)))) (Rget (Val (VArg "_local")) (Val (VStr "_outer")))))) (Rget (Val (VArg "_local")) (Val (VStr "_arg")))))))))]


--reserved = Map.insert "_METATABLE" reservedFunction output

applyOp :: Binop -> Value -> Value -> Either ErrorMsg Value
applyOp Plus (VInt i) (VInt j) = Right $ VInt $ i + j
applyOp Minus (VInt i) (VInt j) = Right $ VInt $ i - j
applyOp Times (VInt i) (VInt j) = Right $ VInt $ i * j
applyOp Divide (VInt i) (VInt j)
              |  j /= 0 = Right $ VInt $ (div i j)
              |  j == 0 = error "[DIVIDE]Don't Divide 0!"
applyOp Gt (VInt i) (VInt j) = Right $ VBool $ i > j
applyOp Ge (VInt i) (VInt j) = Right $ VBool $ i >= j
applyOp Lt (VInt i) (VInt j) = Right $ VBool $ i < j
applyOp Le (VInt i) (VInt j) = Right $ VBool $ i <= j

applyOp Eq (VInt i) (VInt j) = do
  case (i == j) of
    True -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "x"))))
    False -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "y"))))

applyOp Eq (VStr i) (VStr j) = do
  case (i == j) of
    True -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "x"))))
    False -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "y"))))
applyOp Eq (VReg i) (VReg j) = do
  case (i == j) of
    True -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "x"))))
    False -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "y"))))

applyOp Eq val (VNil) = do
  case val of
    VNil -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "x"))))
    otherwise -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "y"))))

applyOp Nq (VInt i) (VInt j) = do
  case (i /= j) of
    True -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "x"))))
    False -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "y"))))

applyOp Nq val (VNil) = do
  case val of
    VNil -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "y"))))
    otherwise -> Right $ VFunc "x" (Val (VFunc "y" (Val (VArg "x"))))



applyOp Power (VInt i) (VInt j) = Right $ VInt $ i ^ j
applyOp Mod (VInt i) (VInt j) = Right $ VInt $ i - (div i j)
applyOp Cont (VStr i) (VStr j) = Right $ VStr $ i ++ j
applyOp And (VBool i) (VBool j) = Right $ VBool $ i && j
applyOp Or (VBool i) (VBool j) = Right $ VBool $ i || j
applyOp Unm (VNil) (VInt j) = Right $ VInt $ -j

applyOp _ _ _ = Left $ "ERROR: [BOP]Wrong Types are not the same"

applyUnop :: Unop -> Value -> Either ErrorMsg Value
applyUnop Neg (VInt i) = Right $ VInt $ -i
applyUnop Not (VBool i) = Right $ VBool $ not i



evaluate :: Expression -> Store -> Either ErrorMsg (Value, Store)

evaluate (Seq e1 e2) s = do
  (_, s') <- evaluate e1 s
  result <- evaluate e2 s'
  return result

evaluate (Val v) s = do
  return (v, s)

evaluate New s = do
  address <- allocateAdr 1 s
  s' <- Right $ Map.insert address Map.empty s
  return ((VReg address), s')

evaluate (Rget table key) s = do
  (a, s1) <- evaluate table s
  (k, s') <- evaluate key s1
  t <- pointToTable a s'
  v <- pointToVal k t
  return (v, s')

evaluate (Rset table key value) s = do
  (a, s1) <- evaluate table s
  (k, s2) <- evaluate key s1
  (v, s3) <- evaluate value s2
  t <- pointToTable a s3
  case k of
    VStr key -> do
      t' <- Right $ Map.insert key v t
      case a of
        VReg reg -> do
          s' <- Right $ Map.insert reg t' s3
          return (a, s')
    otherwise -> Left $ "<ERROR><Rawset> False key type"


evaluate (Opraw exp1 op exp2) s = do
  (v1, s1) <- evaluate exp1 s
  (v2, s') <- evaluate exp2 s1
  v <- applyOp op v1 v2
  return (v, s')

evaluate (Funcall fun pari) s = do
  (exprVal, s1) <- evaluate fun s
  (v1, s2) <- evaluate pari s1
  expr' <- importFunc exprVal v1
  (v, s') <- evaluate expr' s2
  return (v, s')

pointToTable :: Value -> Store -> Either ErrorMsg Table
pointToTable (VReg reg) s = do
  t <- Right $ Map.lookup reg s
  case t of 
    Just table -> return table
    otherwise -> Left $ "<ERROR> Don't have table [" ++ reg ++ "]"
    
pointToVal :: Value -> Table -> Either ErrorMsg Value
pointToVal (VStr k) t = do
  v <- Right $ Map.lookup k t
  case v of
    Just value -> return value
    Nothing -> return VNil
    
pointToVal _ _ = do
  Left $ "<ERROR> FALSE TYPE"
  
  
importFunc :: Value -> Value -> Either ErrorMsg Expression
importFunc (VFunc str expr) val = do
  expr' <- substitute expr str val
  return expr'
importFunc _ val = Left "<ERROR>This is not a function"
  
substitute :: Expression -> String -> Value -> Either ErrorMsg Expression
substitute expr a v = do
  case expr of
    Seq expr1 expr2 -> do
      expr1' <- substitute expr1 a v
      expr2' <- substitute expr2 a v
      return $ Seq expr1' expr2'
    Val value -> do
      case value of
        VFunc arg body -> do
          body' <- substitute body a v
          return $ Val $ VFunc arg body'
        VArg arg -> do
          case (arg == a) of
            True -> return $ Val v
            False -> return $ Val value
        otherwise -> return expr
    New -> return expr
    Rget table key -> do
      table' <- substitute table a v
      key' <- substitute key a v
      return $ Rget table' key'
    Rset table key val -> do
      table' <- substitute table a v
      key' <- substitute key a v
      val' <- substitute val a v
      return $ Rset table' key' val'
    Opraw expr1 op expr2 -> do
      expr1' <- substitute expr1 a v
      expr2' <- substitute expr2 a v
      return $ Opraw expr1' op expr2'
    Funcall func expr -> do
      case func of
        (Val (VFunc arg e)) -> do
          case (arg == a) of
              True -> do
                expr' <- substitute expr a v
                return $ Funcall func expr'
              False -> do
                func' <- substitute func a v
                expr' <- substitute expr a v
                return $ Funcall func' expr'
        otherwise -> do
          func' <- substitute func a v
          expr' <- substitute expr a v
          return $ Funcall func' expr'
    otherwise -> do
      return expr


allocateAdr :: Int -> Store -> Either ErrorMsg Register
allocateAdr num st = do
  address <- Right $ "_#TABLE0" ++ (show num)
  cond <- Right $ Map.notMember address st
  case cond of
    True -> return address
    False -> allocateAdr (succ num) st

-- Executing Method
run :: Expression -> Either ErrorMsg (Value, Store)
run prog = do
  reserve <- Right $ Map.insert "_RESERVE" reservedTable Map.empty
  io <- Right $ Map.insert "_IO" Map.empty reserve
  globle <- Right $ Map.insert "_ENV" globalEnv io
  evaluate prog globle


excuteFile fileName = do
  putStrLn $ "======[THE STORE]======"
  case fileName of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> showStore s
  putStrLn ""
  putStrLn $ "======[THE OUTPUT]======"
  case fileName of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> showStore2 s
  putStrLn ""


showStore s = do
  putStrLn $ Map.showTree $ (s Map.! "_ENV")

showStore2 s = do
  putStrLn $ Map.showTree $ (s Map.! "_IO")

  
  
  
