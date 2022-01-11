module Syntax where
--CompUnit -> FuncDef
--FuncDef  -> FuncType Ident '(' ')' Block
--FuncType -> 'int'
--Ident    -> 'main'
--Block    -> '{' Stmt '}'
--Stmt     -> 'return' Number ';'

data CompUnit = CompUnit FuncDef
  deriving (Show)
data FuncDef = FuncDef FuncType Ident Block
  deriving (Show)
data FuncType = TypeInt
  deriving (Show)
data Ident = Main
  deriving (Show)
data Block = Block Stmt
  deriving (Show)
data Stmt = Return Exp
  deriving (Show)

data Exp = Exp AddExp
  deriving (Show)

data AddExp = AddExp1 MulExp | AddExp2 MulExp Op1 AddExp
  deriving (Show)

data MulExp = MulExp1 UnaryExp | MulExp2 UnaryExp Op2 MulExp
  deriving (Show)

data UnaryExp = UnaryExp1 PrimaryExp | UnaryExp2 Op1 UnaryExp
  deriving (Show)

data PrimaryExp = PrimaryExp1 Exp | PrimaryExp2 Number
  deriving (Show)

type Number = Int

data Op1 = Pos | Neg
  deriving (Eq)

data Op2 = Mul | Div | Mod
  deriving (Eq)

instance Show (Op1) where
  show Pos = "+"
  show Neg = "-"

instance Show (Op2) where
  show Mul = "*"
  show Div = "/"
  show Mod = "%"

