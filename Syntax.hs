module Syntax where
--CompUnit     -> FuncDef
--Decl         -> ConstDecl | VarDecl
--ConstDecl    -> 'const' BType ConstDef { ',' ConstDef } ';'
--BType        -> 'int'
--ConstDef     -> Ident '=' ConstInitVal
--ConstInitVal -> ConstExp
--ConstExp     -> AddExp
--VarDecl      -> BType VarDef { ',' VarDef } ';'
--VarDef       -> Ident
--                | Ident '=' InitVal
--InitVal      -> Exp
--FuncDef      -> FuncType Ident '(' ')' Block // 保证当前 Ident 只为 "main"
--FuncType     -> 'int'
--Block        -> '{' { BlockItem } '}'
--BlockItem    -> Decl | Stmt
--Stmt         -> LVal '=' Exp ';'
--                | [Exp] ';'
--                | 'return' Exp ';'
--Exp          -> AddExp
--LVal         -> Ident
--PrimaryExp   -> '(' Exp ')' | LVal | Number
--AddExp       -> MulExp
--                | AddExp ('+' | '−') MulExp
--MulExp       -> UnaryExp
--                | MulExp ('*' | '/' | '%') UnaryExp
--UnaryExp     -> PrimaryExp
--                | Ident '(' [FuncRParams] ')'
--                | UnaryOp UnaryExp
--FuncRParams  -> Exp { ',' Exp }
--UnaryOp      -> '+' | '-'
data CompUnit = CompUnit FuncDef
  deriving (Show)

data Decl = ConstDecl BType [ConstDef] | VarDecl BType [VarDef]
  deriving (Show)

data BType = BInt
  deriving (Show)

data ConstDef = ConstDef Ident AddExp
  deriving (Show)

data VarDef = VarDef1 Ident | VarDef2 Ident Exp
  deriving (Show)

data FuncDef = FuncDef FuncType Ident Block
  deriving (Show)

data FuncType = FInt
  deriving (Show)

type Ident = String

data Block = Block [BlockItem]

data BlockItem = BlockItem1 Decl | BlockItem2 Stmt
  deriving (Show)

data Stmt = Stmt1 LVal Exp | Stmt2 Exp | SemiColon | Return Exp
  deriving (Show)

type Exp = AddExp

data LVal = LVal Ident
  deriving (Show)

data AddExp = AddExp1 MulExp | AddExp2 MulExp Op1 AddExp

data MulExp = MulExp1 UnaryExp | MulExp2 UnaryExp Op2 MulExp

data UnaryExp = UnaryExp1 PrimaryExp | UnaryExp2 Op1 UnaryExp | UnaryExp3 Ident [Exp]
  deriving (Show)

data PrimaryExp = PrimaryExp1 Exp | PrimaryExp2 Number | PrimaryExp3 LVal
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

instance Show (AddExp) where
  show (AddExp1 (MulExp1 (UnaryExp1 (PrimaryExp2 x)))) = show x
  show (AddExp1 (MulExp1 (UnaryExp3 func es))) = "(UnaryExp3 " ++ show func ++" " ++ show es ++ ")"
  show (AddExp1 (MulExp1 (UnaryExp1 (PrimaryExp3 (LVal var))))) = show var
  show (AddExp1 m) = "(AddExp1 (" ++ show m ++ "))"
  show (AddExp2 m op a) = "(AddExp2 (" ++ show m ++") "++ show op ++" ("++ show a ++ "))"

instance Show (MulExp) where
  show (MulExp1 (UnaryExp1 (PrimaryExp3 (LVal var)))) = show var
  show (MulExp1 u) = "(MulExp1 (" ++ show u ++ "))"
  show (MulExp2 u op m) = "(MulExp2 (" ++ show u ++") "++ show op ++" ("++ show m ++ "))"

instance Show (Block) where
  show (Block []) = "(Block [])"
  show (Block items) = "(Block\n" ++ foldl1 (\a b -> a ++ ",\n" ++ b) (show <$> items) ++ "\n)"

