module SymTab (
  Variable(..),
  Value(..),
  ErrorMsg,
  Store,
  Table,
  Register,
  VType(..),
  Expression(..),
  Unop(..),
  Binop(..),
  Parameter(..),
  ) where
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Unsafe
import System.Environment

type ErrorMsg = String

type Store = Map Register Table

type Table = Map String Value

type Register = String

type Argument = String

data VType = Integer | Boolean | String | Defined
  deriving (Eq, Show)

data Value = 
    VNil
  | VArg String
  | VFunc Argument Expression
  | VResFunc String
  | VReg Register
  | VInt Integer
  | VBool Bool
  | VStr String
  | VTrue
  | VFalse
  deriving (Show)

data Variable = Variable String
  deriving (Show)

data Parameter = Parameter VType
  deriving (Show)
  
data Expression =
    Seq Expression Expression
  | This
  | Val Value                                   --Constant
  | New
  | Rget Expression Expression
  | Rset Expression Expression Expression
  | Opraw Expression Binop Expression
  | Funcall Expression Expression
  | IO Expression
  deriving (Show)
  
data Unop =
    Neg      -- -
  | Not      -- not
  | Num   -- #
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Power    -- ^
  | Mod      -- %
  | Cont     -- ..
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  | Eq       -- == :: Int -> Int -> Bool
  | Nq       -- ~= :: Int -> Int -> Bool
  | And
  | Or
  | Unm
  deriving (Show)



symbolTable :: Store
symbolTable = Map.empty


