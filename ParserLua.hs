module ParserLua (
  parseExp,
  printParseTree
) where
import System.Environment
import SymTab
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Monad.Except
import Control.Monad.ST
import System.Environment
import Data.List
import System.Random

transOp s = case s of
  "+"   -> Plus
  "-"   -> Minus
  "*"   -> Times
  "/"   -> Divide
  ">="  -> Ge
  ">"   -> Gt
  "<="  -> Le
  "<"   -> Lt
  "=="  -> Eq
  "~="  -> Nq
  "and" -> And
  "un"  -> Unm
  "or"  -> Or
  "%"   -> Mod
  "^"   -> Power
  ".."  -> Cont
  o     -> error $ "Unexpected operator " ++ o
  
transUnop s = case s of
  "-"   -> Neg
  "not" -> Not
  "#"   -> Num
  o     -> error $ "Unexpected operator " ++ o

fileP :: GenParser Char st Expression
fileP = do
  prog <- chunk
  eof
  return prog


-- CHUNK ------global environment----------------------------------------
chunk = do
  stats <- block
  return $ Funcall (Funcall (Val (VFunc "_local" (Val (VFunc "_outer"
             stats
           )))) (Val (VReg "_ENV"))) (Val (VReg "_ENV"))


-- BLOCK ----------------------------------------------------------------
block = do
  stat <- try(block') <|> try(returnStatement)
  return $ (Funcall (Val (VFunc "_outer" (Funcall (Val (VFunc "_local"
             stat
           )) (Rset (Rset New (Val (VStr "_outer")) (Val (VArg "_outer"))) (Val (VStr "_arg")) New)))) (Val (VArg "_local")))

block' = do
  spaces
  st <- sequenceStatement
  spaces
  ret <- optionMaybe returnStatement
  return $ case ret of
    Nothing -> st
    Just retSt -> (Funcall (Val (VFunc "" retSt)) (st))

sequenceStatement = do
  spaces
  st <- statement
  spaces
  optional (char ';')
  rest <- optionMaybe restStat
  spaces
  optional (char ';')
  return $ case rest of
    Nothing   -> st
    Just st' -> Funcall (Val (VFunc "" st')) (st)

restStat = do
  try(sequenceStatement)

returnStatement = do
  string "return"
  spaces
  expr <- expression
  spaces
  optional (char ';')
  return expr

-- STATEMENT ------------------------------------------------------------
statement = try(assignmentStatement)
        <|> try(controlStatement)
        <|> try(functionDefinition)
        <|> try(functionApplication)

        <?> "Stat format not supported now..."


reservedFunction = try(printState)
               <|> try(getMetatable)
               <|> try(setMetatable)

getMetatable = do
  string "getmetatable("
  spaces
  expr <- varExpression
  spaces
  char ')'
  spaces
  return $ Rget expr (Val (VStr "_metatable"))

setMetatable = do
  string "setmetatable("
  spaces
  table <- varExpression
  spaces
  char ','
  spaces
  mt <- varExpression
  spaces
  char ')'
  spaces
  return $ Rset table (Val (VStr "_metatable")) mt

printState = do
  string "print"
  spaces
  char '('
  spaces
  expr <- expression
  spaces
  char ')'
  spaces
  return $ Rset (Val (VReg "_IO")) (Val (VStr ("_#IO0" ++ show(1)))) expr

-- STATEMENT.FUNCTION_DEFINITION ----------------------------------------
functionDefinition = try(functionDefWithName)

functionDefWithName = do
  string "function"
  spaces
  funcName <- variable
  spaces
  char '('
  spaces
  arg <- many letter
  spaces
  char ')'
  spaces
  body <- block
  spaces
  string "end"
  return $ setValue funcName (Rset (Rset New (Val (VStr "_arg")) (Rset New (Val (VStr "_1")) (Val (VStr
       arg
      )))) (Val (VStr "_call")) (Val (VFunc "_arg" (Funcall (Val (VFunc "_outer" (Funcall (Val (VFunc "_local"
       body
      )) (Rset (Rset New (Val (VStr "_outer")) (Val (VArg "_outer"))) (Val (VStr "_arg")) (Val (VArg "_arg")))))) (Val (VArg "_local"))))))


-- STATEMENT.FUNCTION_APPLICATION ----------------------------------------

functionApplication = try(reservedFunction) <|> try(functionApp1)

functionApp1 = do
  funcName <- varExpression
  char '('
  spaces
  para <- optionMaybe expression
  spaces
  char ')'
  return $ case para of
    Nothing -> Funcall (Rget funcName (Val (VStr "_call"))) New
    Just st -> Funcall (Rget funcName (Val (VStr "_call"))) (Rset New (Rget (Rget funcName (Val (VStr "_arg"))) (Val (VStr "_1"))) st)


-- STATEMENT.CONTROL -----------------------------------------------------
controlStatement = try(ifStatement)
               <|> try(whileStatement)

-- reserved if function
ifThenElse :: Expression -> Expression -> Expression -> Expression
ifThenElse cond statTrue statFlase =
  Funcall (Funcall (Funcall (Funcall (Val (VFunc "cond" (Val (VFunc "t" (Val (VFunc "f" (Funcall (Funcall (Val (VArg "cond")) (Val (VArg "t"))) (Val (VArg "f")))))))))
    cond ) (Val (VFunc ""
    statTrue ))) (Val (VFunc ""
    statFlase ))) (Val (VArg ""))

-- STATEMENT.CONTROL.IF -----------------------------------------------------

ifStatement = do
  string "if"
  spaces
  expr <- expression
  spaces
  string "then"
  spaces
  thenStat <- block
  spaces
  elseStat <- optionMaybe restElse
  spaces
  string "end"
  spaces
  return $ case elseStat of
    Nothing -> ifThenElse expr thenStat (Val VNil)
    Just st -> ifThenElse expr thenStat st

restStatements = do
  (cond, stat) <- elseIfStatement
  spaces
  rest <- optionMaybe restElse
  return (case rest of
    Nothing -> ifThenElse cond stat (Val VNil)
    Just st -> ifThenElse cond stat st)

restElse = do
    try(restStatements) <|> try(elseStatement)

elseIfStatement = do
  string "elseif"
  spaces
  cond <- expression
  spaces
  string "then"
  spaces
  stat <- block
  spaces
  return $ (cond, stat)

elseStatement = do
  string "else"
  spaces
  rest <- block
  spaces
  return rest


-- STATEMENT.CONTROL.WHILE --------------------------------------------
whileDoEnd :: Expression -> Expression -> Expression
whileDoEnd cond expr = (Funcall (Val (VFunc "" (Funcall (Rget (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "getValue"))) (Val (VStr "while"))) (Val (VArg "_local"))) (Val (VStr "_call"))) New))) (Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "setValue"))) (Val (VStr "while"))) (Rset (Rset New (Val (VStr "_arg")) (Rset New (Val (VStr "_1")) (Val (VStr "")))) (Val (VStr "_call")) (Val (VFunc "_arg" (Funcall (Val (VFunc "_outer" (Funcall (Val (VFunc "_local" (Funcall (Val (VFunc "_outer" (Funcall (Val (VFunc "_local" (Funcall (Funcall (Funcall (Funcall (Val (VFunc "cond" (Val (VFunc "t" (Val (VFunc "f" (Funcall (Funcall (Val (VArg "cond")) (Val (VArg "t"))) (Val (VArg "f")))))))))
      cond
    ) (Val (VFunc "" (Funcall (Val (VFunc "_outer" (Funcall (Val (VFunc "_local" (Funcall (Val (VFunc "" (Funcall (Rget (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "getValue"))) (Val (VStr "while"))) (Val (VArg "_local"))) (Val (VStr "_call"))) New)))
      expr
    ))) (Rset (Rset New (Val (VStr "_outer")) (Val (VArg "_outer"))) (Val (VStr "_arg")) New)))) (Val (VArg "_local")))))) (Val (VFunc "" (Val VNil)))) (Val (VArg ""))))) (Rset (Rset New (Val (VStr "_outer")) (Val (VArg "_outer"))) (Val (VStr "_arg")) New)))) (Val (VArg "_local"))))) (Rset (Rset New (Val (VStr "_outer")) (Val (VArg "_outer"))) (Val (VStr "_arg")) (Val (VArg "_arg")))))) (Val (VArg "_local"))))))) (Val (VArg "_local"))))




whileStatement = do
  string "while"
  spaces
  cond <- expression
  spaces
  string "do"
  spaces
  stat <- block
  spaces
  string "end"
  spaces
  return $ whileDoEnd cond stat




-- EXPRESSION -------------------------------------------------------
expression = try(tableConst)
         <|> try(binOpExpression)   --include constant expr
         <|> try(prefixExpression)


-- EXPRESSION.BINOP -------------------------------------------------
binOpExpression = buildExpressionParser opRule opSubExpression

opSubExpression = do
  spaces
  expr <- try(prefixExpression)
      <|> try(constantExpr)
  spaces
  return expr

opRule = [ [binary "==" AssocLeft, binary "~=" AssocLeft,
            binary "<=" AssocLeft, binary ">=" AssocLeft,
            binary "<" AssocLeft, binary ">" AssocLeft]
         , [binary "*" AssocLeft, binary "/" AssocLeft ]
         , [binary "+" AssocLeft, binary "-" AssocLeft ] ]

binary name assoc = Infix (string name >> return (translateOp name)) assoc

translateOp :: String -> Expression -> Expression -> Expression
translateOp "==" expr1 expr2 = Opraw expr1 Eq expr2
translateOp "<=" expr1 expr2 = Opraw expr1 Le expr2
translateOp "<" expr1 expr2 = Opraw expr1 Lt expr2
translateOp "~=" expr1 expr2 = Opraw expr1 Nq expr2
translateOp ">=" expr1 expr2 = Opraw expr1 Ge expr2
translateOp ">" expr1 expr2 = Opraw expr1 Gt expr2
translateOp "+" expr1 expr2 = Opraw expr1 Plus expr2
translateOp "-" expr1 expr2 = Opraw expr1 Minus expr2
translateOp "*" expr1 expr2 = Opraw expr1 Times expr2
translateOp "/" expr1 expr2 = Opraw expr1 Divide expr2

-- EXPRESSION.BINOP.CONSTANT ----------------------------------------
constantExpr = do
  spaces
  t <- numberTerm
   <|> try(stringTerm)
   <|> try(nilTerm)
  spaces
  return $ Val t

numberTerm = do
  num <- many1 digit
  return $ VInt $ read num

stringTerm = do
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ VStr str

nilTerm = do
  string "nil"
  return VNil


-- EXPRESSION.BINOP.PREFIX -------------------------------------------
prefixExpression = try(functionApplication)
               <|> try(anonymousFunction)
               <|> try(varExpression)



-- EXPRESSION.BINOP.PREFIX.ANONYMOUS_FUNCTION ------------------------
anonymousFunction = do
  string "function"
  spaces
  char '('
  arg <- many $ letter
  char ')'
  spaces
  body <- block
  spaces
  string "end"
  return $ Rset (Rset New (Val (VStr "_arg")) (Rset New (Val (VStr "_1")) (Val (VStr
           arg
           )))) (Val (VStr "_call")) (Val (VFunc "_arg" (Funcall (Val (VFunc "_outer" (Funcall (Val (VFunc "_local"
           body
           )) (Rset (Rset New (Val (VStr "_outer")) (Val (VArg "_outer"))) (Val (VStr "_arg")) (Val (VArg "_arg")))))) (Val (VArg "_local")))))


-- EXPRESSION.BINOP.PREFIX.VARIABLE ----------------------------------
varExpression = try(prefixVarExpr)
            <|> try(singleVarExpr)
            <?> "Wrong Expression"

-- reserved getvalue function ---------------
getValue :: Expression -> Expression
getValue expr = Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "getValue"))) expr) (Val (VArg "_local"))

singleVarExpr = firstVar

prefixVarExpr = do
  head <- firstVar
  body <- getBody
  spaces
  return $ modifyGetExpr head body

modifyGetExpr :: Expression -> Expression -> Expression
modifyGetExpr head (Rget expr1 expr2) = case expr1 of
  Rget _ _ -> Rget (modifyGetExpr head expr1) expr2
  otherwise -> Rget (Rget head expr1) expr2
modifyGetExpr head (Val val) = Rget head (Val val)

getBody = buildExpressionParser getRule postfixVariable
getRule = [ [Infix (return Rget) AssocLeft] ]

postfixVariable = try(brckPost) <|> try(dotPost)

dotPost = do
  char '.'
  var <- variable
  return var

brckPost = do
  char '['
  spaces
  var <- try(stringTerm) <|> try(numberField)
  spaces
  char ']'
  return $ Val var

firstVar = do
  var <- variable
  return $ getValue var

variable = do
  first <- letter
  str <- many $ alphaNum
  return $ Val (VStr (concat[[first], str]))

-- EXPRESSION.TABLECONST -------------------------------------------
tableConst = try(tableNew)
         <|> try(tableCon)

tableNew = do
  string "{}"
  spaces
  return $ New

tableCon = do
  char '{'
  spaces
  raw <- fieldList
  spaces
  char '}'
  spaces
  return $ translateField raw

-- MENTION: The node <Rget> is just for convinience, not for final result
fieldList = buildExpressionParser [ [Infix (char ',' >> return Rget) AssocLeft] ] field


translateField :: Expression -> Expression
translateField (Rget expr1 expr2) = case expr1 of
  Rget _ _ -> case expr2 of
    Rget var val -> Rset (translateField expr1) var val
  otherwise -> Rset New expr1 expr2

field = do
  spaces
  expr <- try(assignDotField)
      <|> try(assignBrcField)
  spaces
  return expr

-- MENTION: The node <Rget> is just for convinience, not for final result
assignDotField = do
  var <- variable
  spaces
  char '='
  spaces
  val <- expression
  return $ Rget var val

assignBrcField = do
  char '['
  spaces
  var <- try(stringTerm) <|> try(numberField)
  spaces
  char ']'
  spaces
  char '='
  spaces
  val <- expression
  return $ Rget (Val var) val

numberField = do
  var <- many1 digit
  return $ VStr ("_" ++ var)


-- STATEMENT.ASSIGN ---------------------------------------------------------

assignmentStatement = try(localAssignStat)
                  <|> try(singleAssignStat)
                  <|> try(prefixAssignStat)
                  <?> "Stat format not supported now..."

-- reserved function for setValue ---------------------------------
setValue :: Expression -> Expression -> Expression
setValue var value = Funcall (Funcall (Funcall (Rget (Val (VReg "_RESERVE")) (Val (VStr "setValue"))) var) value) (Val (VArg "_local"))

-- STATEMENT.ASSIGN.SINGLE ---------------------------------------------------------
singleAssignStat = do
  var <- variable
  spaces
  char '='
  spaces
  val <- expression
  spaces
  return $ setValue var val

-- STATEMENT.ASSIGN.NORMAL ---------------------------------------------------------
prefixAssignStat = do
  head <- firstVar
  rawBody <- tableKey
  spaces
  char '='
  spaces
  val <- expression
  spaces
  return $ case (modifySetExpr head rawBody) of
    (table, key) -> Rset table key val

-- STATEMENT.ASSIGN.LOCAL ---------------------------------------------------------
localAssignStat = do
  string "local"
  spaces
  var <- variable
  spaces
  char '='
  spaces
  val <- expression
  spaces
  return $ Rset (Val (VArg "_local")) var val



-- MENTION: The node <Rget> is just for convinience, not for final result
tableKey = buildExpressionParser [ [Infix (return Rget) AssocLeft] ] postfixVariable

modifySetExpr :: Expression -> Expression -> (Expression, Expression)
modifySetExpr head (Rget expr1 expr2) = (modifyGetExpr head expr1, expr2)
modifySetExpr head body = (head, body)





-- Parsing Method ---------------------------------------------------------
parseExp fileName = do
  p <- parseFromFile fileP fileName
  return p

printParseTree fileName = do
  p <- parseExp fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> do
      putStrLn $ "======[AST FOR EXP]======"
      print exp
      putStrLn $ ""
        


