{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Emit where

import Syntax
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

emit :: CompUnit -> String
emit (CompUnit f) =
  let ret = runState (runCodegen (funcDef f))
                     (CodegenState { symTab = []
                                   , constTab = Map.empty
                                   , curBlkName = 0
                                   , blkTab = Map.empty
                                   , calledFuncs = Map.empty})
   in Map.foldr (++) "" (calledFuncs $ snd $ ret) ++ fst ret

class OpCode a where
  opCode :: a -> String
  evalConstOp :: a -> Int -> Int -> Int
instance OpCode AddOp where
  opCode Add = "add"
  opCode Sub = "sub"
  evalConstOp Add a b = (a + b)
  evalConstOp Sub a b = (a - b)
instance OpCode MulOp where
  opCode Mul = "mul"
  opCode Div = "sdiv"
  opCode Mod = "srem"
  evalConstOp Mul a b = (a * b)
  evalConstOp Div a b = (a `div` b)
  evalConstOp Mod a b = (a `rem` b)
instance OpCode UnaryOp where
  opCode LNot = "TODO"
  opCode Pos = "add"
  opCode Neg = "sub"
  evalConstOp Pos 0 b = b
  evalConstOp Neg 0 b = -b
  evalConstOp LNot 0 b = error "evalConstExp LNot not implemented because bool type is not supported"
  evalConstOp _ _ _ = error "UnaryOp should be called with the first argument 0"

tripleCode :: String -> String -> String -> String -> String
tripleCode op dest src1 src2  = "    " ++ dest ++ " = " ++ op ++ " i32 "
  ++ src1 ++ ", " ++ src2 ++ "\n"

-- Codegen state

type Name = String
type Symbol = String
type VarName = String
type BlockName = Int
-- VarName could be "" when a symbol refers to a temp variable other than a
-- variable in source codes.
type SymbolTable = [(VarName, Symbol)]
type ConstTable = Map.Map VarName Int

data CodegenState = CodegenState {
    symTab :: SymbolTable
  , constTab :: ConstTable
  , curBlkName :: BlockName
  , blkTab :: Map.Map BlockName BlockState
  , calledFuncs :: Map.Map String String
  } deriving (Show)

data BlockState = BlockState {
    blkSym :: String  -- could be ""
  , closureBlkNames :: [BlockName]
  } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- Helper functions
lastSym symtab = snd (head symtab)

newSym [] = "%1"
newSym symtab = "%" ++ show (read (tail $ snd $ head symtab) + 1)

appendSym :: VarName -> Symbol -> SymbolTable -> SymbolTable
appendSym var sym sym_tab = (var, sym):sym_tab

-- Codegen

funcDef :: FuncDef -> Codegen String
funcDef (FuncDef t id b) = do
  b' <- block b ""
  let type_code = case t of FInt -> "i32"
      head_code = case id of "main" -> "define dso_local "++type_code++" @main(){\n"
                             _ -> error "the compiler only support main function now"
  return $ head_code ++ b' ++ "}"

block :: Block -> String -> Codegen String
block (Block items) blk_sym= do
  st <- get
  let bs = blkTab st
      cur_blk_name = curBlkName st
      new_blk_name = newBlockName cur_blk_name
      bs' = Map.insert
        new_blk_name
        (BlockState {blkSym = blk_sym, closureBlkNames = getClosureBlockNames cur_blk_name bs})
        bs
  put (st {blkTab = bs'})

  items' <- blockItems items
  return $ foldl1 (++) items'
  where getClosureBlockNames 0 bs = []
        getClosureBlockNames cur_blk_name bs = cur_blk_name:(closureBlkNames (bs Map.! cur_blk_name))
        newBlockName blk_name = blk_name + 1

blockItems :: [BlockItem] -> Codegen [String]
blockItems [] = return []
blockItems (bi:bis) = do
  bi' <- blockItem bi
  bis' <- blockItems bis
  return $ bi':bis'

blockItem :: BlockItem -> Codegen String
blockItem (BlockItem1 decl') = decl decl'
blockItem (BlockItem2 stmt') = stmt stmt'


decl :: Decl -> Codegen String

decl (ConstDecl BInt []) = return ""

decl (ConstDecl BInt ((ConstDef id e):cds)) = do
  const_tab <- gets constTab
  case Map.lookup id const_tab of Nothing -> return ""
                                  Just _ -> error $ "const variable "++id++" already exists"
  let e_int = case evalConstExp e const_tab of Right r -> r
                                               Left l -> error $ show l++"\n    in "++show e
      new_const_tab = Map.insert id e_int const_tab
  if e_int == e_int  -- force evaluating the thunk e_int
  then do modify (\st -> st {constTab = new_const_tab})
          decl (ConstDecl BInt cds)
  else return ""


decl (VarDecl BInt []) = return ""

decl (VarDecl BInt ((VarDef1 id):vds)) = do
  sym_tab <- gets symTab
  case lookup id sym_tab of Nothing -> return ""
                            Just _ -> error $ "variable "++id++" already exists"
  let new_sym = newSym sym_tab
  modify (\st -> st {symTab = appendSym id new_sym sym_tab})
  remaining_codes <- decl (VarDecl BInt vds)
  return $ "    "++new_sym++" = alloca i32\n"++remaining_codes

decl (VarDecl BInt ((VarDef2 id e):vds)) = do
  decl_codes <- decl (VarDecl BInt [VarDef1 id])

  sym_var <- gets (lastSym . symTab)

  exp_codes <- expr e

  sym_src <- gets (lastSym . symTab)

  remaining_codes <- decl (VarDecl BInt vds)

  return $ decl_codes++exp_codes++"    store i32 "++sym_src++", i32* "++sym_var++"\n"++remaining_codes

stmt :: Stmt -> Codegen String
stmt (Stmt1 (LVal id) e) = do
  exp_codes <- expr e
  sym_tab <- gets symTab
  const_tab <- gets constTab
  let lvar_sym = case lookup id sym_tab of Just j -> j
                                           Nothing -> error $ "LVal "++id++" not found"
  return $ exp_codes++"    store i32 "++lastSym sym_tab++", i32* "++lvar_sym++"\n"
stmt (Stmt2 e) = expr e
stmt StmtSemiColon = return ""
stmt (StmtReturn e) = do
  exp_codes <- expr e
  sym <- gets (lastSym . symTab)
  return $ exp_codes++"    ret i32 "++sym++"\n"

-- Evaluate const expressions
evalConstExp e const_tab = evalConstAddExp e const_tab

evalConstAddExp :: AddExp -> ConstTable -> Either String Int
evalConstAddExp (AddExp1 m) const_tab = evalConstMulExp m const_tab
evalConstAddExp (AddExp2 m op a) const_tab = liftM2 (evalConstOp op) (evalConstMulExp m const_tab) (evalConstAddExp a const_tab)

evalConstMulExp :: MulExp -> ConstTable -> Either String Int
evalConstMulExp (MulExp1 u) const_tab = evalConstUnaryExp u const_tab
evalConstMulExp (MulExp2 u op m) const_tab = liftM2 (evalConstOp op) (evalConstUnaryExp u const_tab) (evalConstMulExp m const_tab)

evalConstUnaryExp :: UnaryExp -> ConstTable -> Either String Int
evalConstUnaryExp (UnaryExp1 p) const_tab = evalConstPrimaryExp p const_tab
evalConstUnaryExp (UnaryExp2 op u) const_tab = liftM (evalConstOp op 0) (evalConstUnaryExp u const_tab)
evalConstUnaryExp (UnaryExpCallFunc i _) const_tab = Left "using function call in a const context"

evalConstPrimaryExp :: PrimaryExp -> ConstTable -> Either String Int
evalConstPrimaryExp (PrimaryExp1 e) const_tab = evalConstExp e const_tab
evalConstPrimaryExp (PrimaryExp2 n) const_tab = return n
evalConstPrimaryExp (PrimaryExp3 (LVal id)) const_tab =
  case Map.lookup id const_tab of Just v -> Right v
                                  Nothing -> Left "using non-const variable in a const context"

-- Expressions

newSymCode :: (OpCode a) => a -> String -> String -> Codegen String
newSymCode op operand1 operand2 = do
  sym_tab <- gets symTab
  let new_sym = newSym sym_tab
      new_code = tripleCode (opCode op) new_sym operand1 operand2
  modify (\st -> st {symTab = appendSym "" new_sym sym_tab})
  return new_code

expr :: Exp -> Codegen String
expr e = addExp e


addExp :: AddExp -> Codegen String

addExp (AddExp1 m) = mulExp m

addExp (AddExp2 m1 op a2)
  | isNum m1 && isNum a2 = newSymCode op (show . getNum $ m1) (show . getNum $ a2)
  | isNum m1 = do
    a2_codes <- addExp a2
    a2_sym <- gets (lastSym . symTab)
    new_code <- newSymCode op (show . getNum $ m1) a2_sym
    return $ a2_codes++new_code
  | otherwise = do
    m1_codes <- mulExp m1
    m1_sym <- gets (lastSym . symTab)
    a2_codes <- addExp a2
    a2_sym <- gets (lastSym . symTab)
    new_code <- newSymCode op m1_sym a2_sym
    return $ m1_codes++a2_codes++new_code


mulExp :: MulExp -> Codegen String

mulExp (MulExp1 u) = unaryExp u

mulExp (MulExp2 u1 op m2)
  | isNum u1 = do
    m2_codes <- mulExp m2
    m2_sym <- gets (lastSym . symTab)
    new_code <- newSymCode op (show . getNum $ u1) m2_sym
    return $ m2_codes++new_code
  | otherwise = do
    u1_codes <- unaryExp u1
    u1_sym <- gets (lastSym . symTab)
    m2_codes <- mulExp m2
    m2_sym <- gets (lastSym . symTab)
    new_code <- newSymCode op u1_sym m2_sym
    return $ u1_codes++m2_codes++new_code


unaryExp :: UnaryExp -> Codegen String

unaryExp (UnaryExp1 (PrimaryExp1 e)) = expr e
unaryExp (UnaryExp1 (PrimaryExp2 n)) = newSymCode Pos "0" (show n)
unaryExp (UnaryExp1 (PrimaryExp3 (LVal id))) = do
  sym_tab <- gets symTab
  const_tab <- gets constTab
  let lvar_sym = lookup id sym_tab
      const_var = Map.lookup id const_tab
      new_sym = newSym sym_tab
  modify (\st -> st {symTab = appendSym "" new_sym sym_tab})
  case lvar_sym of Just j -> return $ "    "++new_sym++" = load i32, i32* "++j++"\n"
                   Nothing -> case const_var of Just cv -> return $ "    "++new_sym++" = add i32 0, "++show cv++"\n"
                                                Nothing -> error $ "LVal "++id++" is neither variable or const variable"

unaryExp (UnaryExp2 op (UnaryExp1 p)) =
  case p of (PrimaryExp1 e) -> do
              e_codes <- expr e
              e_sym <- gets (lastSym . symTab)
              new_code <- newSymCode op "0" e_sym
              return $ e_codes++new_code
            (PrimaryExp2 n) -> newSymCode op "0" (show n)
            (PrimaryExp3 (LVal id)) -> do
              u_codes <- unaryExp (UnaryExp1 p)
              u_sym <- gets (lastSym . symTab)
              new_code <- newSymCode op "0" u_sym
              return $ u_codes++new_code


unaryExp (UnaryExp2 op u) = do
  u_codes <- unaryExp u
  u_sym <- gets (lastSym . symTab)
  new_code <- newSymCode op "0" u_sym
  return $ u_codes++new_code

-- evaluting functions
unaryExp (UnaryExpCallFunc id es) =
  case lookup id libFuncs of
    Nothing -> error $ "function "++id++" is not defined"
    Just declaration -> do
      cfs <- gets calledFuncs
      modify (\st -> st {calledFuncs = Map.insert id declaration cfs})
      if (id == "getint" || id == "getch") && length es == 0
      then do
        sym_tab <- gets symTab
        let new_sym = newSym sym_tab
        modify (\st -> st {symTab = appendSym "" new_sym sym_tab})
        return $ "    "++new_sym++" = call i32 @"++id++"()\n"
      else if (id == "putint" || id == "putch") && length es == 1
           then do
             e_codes <- expr (head es)
             e_sym <- gets (lastSym . symTab)
             return $ e_codes++"    call void @"++id++"(i32 "++e_sym++")\n"
           else
             error $ "arguments number is not correct:"++show (length es)

libFuncs = [("getint", "declare i32 @getint()\n"), ("getch", "declare i32 @getch()\n"),
            ("putint", "declare void @putint(i32)\n"), ("putch", "declare void @putch(i32)\n")]

