module Syntax where
--CompUnit -> FuncDef
--FuncDef  -> FuncType Ident '(' ')' Block
--FuncType -> 'int'
--Ident    -> 'main'
--Block    -> '{' Stmt '}'
--Stmt     -> 'return' Number ';'

data CompUnit = CompUnit FuncDef deriving(Eq, Ord, Show)
data FuncDef = FuncDef FuncType Ident Block deriving(Eq, Ord, Show)
data FuncType = TypeInt deriving(Eq, Ord, Show)
data Ident = Main deriving(Eq, Ord, Show)
data Block = Block Stmt deriving(Eq, Ord, Show)
data Stmt = Return Number deriving(Eq, Ord, Show)
type Number = Int

