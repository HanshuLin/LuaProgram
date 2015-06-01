import ParserLua
import Executer
import SymTab
import System.IO.Unsafe
import Data.IORef
import Data.Either

code = "rawcode.imp"
  
test code = do
  putStrLn $ "***Testing: " ++ code ++ " ***"
  putStrLn $ ""
  printParseTree code
  t <- parseExp code
  excuteFile t
  putStrLn ""

run :: IO ()
run = do
  test "testlua.imp"