module Ast (
  _ostream_mem_loc,
  _ref_mem_loc,
  _ref_mem_idx,
  _heap_disply_pad,
  _disply_pad_char,
  Identifier,
  ErrorMsg,
  Store,
  Statement(..),
  Expression(..),
  Variable(..),
  Unaop(..),
  Binop(..),
  Value(..)
) where

import Data.Map (Map)

-- Interpreter Configuration
_ostream_mem_loc = "system.reserved.ostream"

-- Assume the memory address start from location 0x400 (or 1024)
_ref_mem_loc = "system.reserved.ref.pointer"
_ref_mem_idx = 262144 :: Int

_heap_disply_pad = 10 :: Int
_disply_pad_char = ' '

-- We represent variables as strings.
type Identifier = String
type MemAddress = Int

--We also represent error messages as strings.
type ErrorMsg = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Identifier Value

data Statement =
    Assign Expression Expression
  | Sequence Statement Statement
  | IfElse Expression Statement Statement
  | If Expression Statement
  | While Expression Statement
  | DoWhile Statement Expression
  | For Statement Expression Statement Statement
  | Log Expression
  | Block Statement
  | FuncDef Identifier [Identifier] Statement
  | FuncApp Expression
  | Return Expression
  | Skip
  deriving (Show)

data Expression =
    Var Variable
  | Val Value
  | Func Identifier [Expression]
  | Uop Unaop Expression
  | Bop Binop Expression Expression
  deriving (Show)

data Variable =
    StdVar Identifier
  deriving (Show)

data Unaop =
    Not
  deriving (Show)

data Binop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Seq
  | Eq
  | Sneq
  | Neq
  | And
  | Or
  | Dot
  | Idx
  deriving (Show)

data Value =
    UdfVal
  | NullVal
  | NaNVal
  | IntVal Int
  | BoolVal Bool
  | StrVal String
  | AryVal [Value]
  | ObjVal [(String, Value)]
  | RefVal MemAddress
  | FuncVal [Identifier] Statement
  deriving (Show)
