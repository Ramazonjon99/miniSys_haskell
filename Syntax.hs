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
--                | Block
--                | [Exp] ';'
--                | 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
--                | 'return' Exp ';' // [changed]
--Exp          -> AddExp
--Cond         -> LOrExp // [new]
--LVal         -> Ident
--PrimaryExp   -> '(' Exp ')' | LVal | Number
--UnaryExp     -> PrimaryExp
--                | Ident '(' [FuncRParams] ')'
--                | UnaryOp UnaryExp
--UnaryOp      -> '+' | '-' | '!'  // 保证 '!' 只出现在 Cond 中 [changed]
--FuncRParams  -> Exp { ',' Exp }
--MulExp       -> UnaryExp
--                | MulExp ('*' | '/' | '%') UnaryExp
--AddExp       -> MulExp
--                | AddExp ('+' | '-') MulExp
--RelExp       -> AddExp
--                | RelExp ('<' | '>' | '<=' | '>=') AddExp  // [new]
--EqExp        -> RelExp
--                | EqExp ('==' | '!=') RelExp  // [new]
--LAndExp      -> EqExp
--                | LAndExp '&&' EqExp  // [new]
--LOrExp       -> LAndExp
--                | LOrExp '||' LAndExp  // [new]

data CompUnit = CompUnit FuncDef
  deriving (Show)

data Decl = ConstDecl BType [ConstDef] | VarDecl BType [VarDef]
  deriving (Show)

data ConstDef = ConstDef Ident AddExp
  deriving (Show)

data BType = BInt
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

data Stmt = Stmt1 LVal Exp
          | Stmt2 Exp
          | StmtSemiColon
          | StmtReturn Exp
          | StmtIfElse Cond Stmt Stmt  -- the second stmt will be StmtSemiColon if the `else` branch is missing
          | StmtBlock Block
  deriving (Show)

type Exp = AddExp

type Cond = LOrExp

data LVal = LVal Ident
  deriving (Show)

data AddExp = AddExp1 MulExp | AddExp2 MulExp AddOp AddExp

data MulExp = MulExp1 UnaryExp | MulExp2 UnaryExp MulOp MulExp

data UnaryExp = UnaryExp1 PrimaryExp | UnaryExp2 UnaryOp UnaryExp | UnaryExpCallFunc Ident [Exp]
  deriving (Show)

data PrimaryExp = PrimaryExp1 Exp | PrimaryExp2 Number | PrimaryExp3 LVal
  deriving (Show)

data LOrExp = LOrExp1 LAndExp | LOrExp2 LAndExp LOrExp
  deriving (Show)

data LAndExp = LAndExp1 EqExp | LAndExp2 EqExp LAndExp
  deriving (Show)

data EqExp = EqExp1 RelExp | EqExp2 RelExp String EqExp
  deriving (Show)

data RelExp = RelExp1 AddExp | RelExp2 AddExp String RelExp
  deriving (Show)

type Number = Int

data AddOp = Add | Sub
  deriving (Show, Eq)

data MulOp = Mul | Div | Mod
  deriving (Show, Eq)

data UnaryOp = LNot | Pos | Neg
  deriving (Show, Eq)

instance Show (AddExp) where
  show (AddExp1 (MulExp1 (UnaryExp1 (PrimaryExp2 x)))) = show x
  show (AddExp1 (MulExp1 (UnaryExpCallFunc func es))) = "(UnaryExpCallFunc " ++ show func ++" " ++ show es ++ ")"
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

class IsNumber a where
  isNum :: a -> Bool
  getNum :: a -> Int

instance IsNumber MulExp where
  isNum (MulExp1 (UnaryExp1 (PrimaryExp2 n))) = True
  isNum _ = False
  getNum (MulExp1 (UnaryExp1 (PrimaryExp2 n))) = n
  getNum _ = error "the MulExp is not a number"

instance IsNumber AddExp where
  isNum (AddExp1 (MulExp1 (UnaryExp1 (PrimaryExp2 n)))) = True
  isNum _ = False
  getNum (AddExp1 (MulExp1 (UnaryExp1 (PrimaryExp2 n)))) = n
  getNum _ = error "the AddExp is not a number"

instance IsNumber UnaryExp where
  isNum (UnaryExp1 (PrimaryExp2 n)) = True
  isNum _ = False
  getNum (UnaryExp1 (PrimaryExp2 n)) = n
  getNum _ = error "the UnaryExp is not a number"
