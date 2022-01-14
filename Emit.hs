{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Emit where

import Syntax
import Data.Char (ord, chr)
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

emit :: CompUnit -> String
emit cu =
  let ret = runState (runCodegen (compUnit cu))
                     (CodegenState { symTab = []
                                   , curBlkName = 0
                                   , blkTab = Map.empty
                                   , calledFuncs = Map.empty})
   in Map.foldr (++) "" (calledFuncs $ snd $ ret) ++ fst ret -- ++ (show . snd)  ret

class OpCode a where
  opInstr :: a -> String
  evalConstOp :: a -> Int -> Int -> Int
instance OpCode AddOp where
  opInstr Add = "add"
  opInstr Sub = "sub"
  evalConstOp Add a b = (a + b)
  evalConstOp Sub a b = (a - b)
instance OpCode MulOp where
  opInstr Mul = "mul"
  opInstr Div = "sdiv"
  opInstr Mod = "srem"
  evalConstOp Mul a b = (a * b)
  evalConstOp Div a b = (a `div` b)
  evalConstOp Mod a b = (a `rem` b)
instance OpCode UnaryOp where
  opInstr Pos = "add"
  opInstr Neg = "sub"
  evalConstOp Pos 0 b = b
  evalConstOp Neg 0 b = -b
  evalConstOp LNot 0 b = error "evalConstExp LNot not implemented because bool type is not supported"
  evalConstOp _ _ _ = error "UnaryOp should be called with the first argument 0"

cmpInstr "==" = "eq"
cmpInstr "!=" = "ne"
cmpInstr "<=" = "sle"
cmpInstr ">=" = "sge"
cmpInstr "<" = "slt"
cmpInstr ">" = "sgt"

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

type VarTable = Map.Map VarName Symbol
type ConstVarTable = Map.Map VarName Int

data CodegenState = CodegenState {
    symTab :: SymbolTable
  , curBlkName :: BlockName  -- -1 for global
  , blkTab :: Map.Map BlockName BlockState
  , calledFuncs :: Map.Map String String
  } deriving (Show)

data BlockState = BlockState {
    blkSym :: String  -- could be ""
  , blkVarTab :: VarTable  -- variables declared in this block
  , blkConstVarTab :: ConstVarTable  -- const variables declared in this block
  , closureBlkNames :: [BlockName]
  } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- Helper functions
lastSym symtab = snd (head symtab)

newSym [] = "%1"
newSym symtab = "%" ++ show (read (tail $ snd $ head symtab) + 1)

newSym' sym = "%" ++ show (read (tail sym) + 1)

newGlobalSym :: VarTable -> Symbol
newGlobalSym var_tab =
  if Map.null var_tab
  then "@a"
  else Map.foldr (\a b -> nextSym' (max a b)) "@a" var_tab
  where nextSym' s
          | last s == 'z' = s++"a"
          | otherwise = init s ++ [chr ((ord $ last s) + 1)]

appendSym :: VarName -> Symbol -> SymbolTable -> SymbolTable
appendSym var sym sym_tab = (var, sym):sym_tab

iterateClosures :: (BlockState -> Maybe b) -> BlockName -> CodegenState -> Maybe b
iterateClosures f blk_name st =
  let cur_blk = blkTab st Map.! blk_name
   in case f cur_blk of
        Just j -> Just j
        Nothing -> case closureBlkNames cur_blk of
          [] -> Nothing
          _ -> iterateClosures f (head $ closureBlkNames cur_blk) st

lookupVarRecur id st =
  let blk_name = curBlkName st
   in iterateClosures (lookupVar' id) blk_name st
  where lookupVar' id blk = Map.lookup id (blkVarTab blk)

lookupConstVarRecur id st =
  let blk_name = curBlkName st
   in iterateClosures (lookupConstVar' id) blk_name st
  where lookupConstVar' id blk = Map.lookup id (blkConstVarTab blk)

lookupVar id st =
  let blk_name = curBlkName st
      cur_blk = blkTab st Map.! blk_name
   in Map.lookup id (blkVarTab cur_blk)

lookupConstVar id st =
  let blk_name = curBlkName st
      cur_blk = blkTab st Map.! blk_name
   in Map.lookup id (blkConstVarTab cur_blk)

insertVar :: VarName -> Symbol -> CodegenState -> CodegenState
insertVar id sym st =
  let blk_name = curBlkName st
      blk_tab = blkTab st
      cur_blk = blk_tab Map.! blk_name
      blk_var_tab = blkVarTab cur_blk
      new_blk = cur_blk {blkVarTab = Map.insert id sym blk_var_tab}
      new_blk_tab = Map.insert blk_name new_blk blk_tab
   in st {blkTab = new_blk_tab}

insertConstVar :: VarName -> Int -> CodegenState -> CodegenState
insertConstVar id n st =
  let blk_name = curBlkName st
      blk_tab = blkTab st
      cur_blk = blk_tab Map.! blk_name
      blk_const_var_tab = blkConstVarTab cur_blk
      new_blk = cur_blk {blkConstVarTab = Map.insert id n blk_const_var_tab}
      new_blk_tab = Map.insert blk_name new_blk blk_tab
   in st {blkTab = new_blk_tab}

-- Codegen

compUnit :: CompUnit -> Codegen String
compUnit (CompUnit global_decls f) = do
  blk_tab <- gets blkTab
  modify (\st -> st { blkTab = Map.insert (-1) (BlockState { blkSym = ""
                                                           , blkVarTab = Map.empty
                                                           , blkConstVarTab = Map.empty
                                                           , closureBlkNames = []}) blk_tab
                    , curBlkName = -1})
  liftM2 (++) (globalDecls global_decls) (funcDef f)

globalDecls [] = return ""
globalDecls (gd:gds) = liftM2 (++) (globalDecl gd) (globalDecls gds)


globalDecl (ConstDecl BInt cds) = decl (ConstDecl BInt cds)

globalDecl (VarDecl BInt []) = return ""

globalDecl (VarDecl BInt ((VarDef1 id):vds)) = do
  st <- get
  case lookupVar id st of Nothing -> return ""
                          Just _ -> error $ "variable "++id++" already exists"
  let var_tab = blkVarTab (blkTab st Map.! (-1))
      new_sym = newGlobalSym var_tab
  modify (insertVar id new_sym)

  remaining_codes <- globalDecl (VarDecl BInt vds)
  return $ new_sym++" = dso_local global i32 0\n"++remaining_codes

globalDecl (VarDecl BInt ((VarDef2 id e):vds)) = do
  st <- get
  case lookupVar id st of Nothing -> return ""
                          Just _ -> error $ "variable "++id++" already exists"
  let var_tab = blkVarTab (blkTab st Map.! (-1))
      new_sym = newGlobalSym var_tab
      e_int = case evalConstExp e st of Right r -> r
                                        Left l -> error $ show l++"\n    in "++show e
  modify (insertVar id new_sym)

  remaining_codes <- globalDecl (VarDecl BInt vds)
  return $ new_sym++" = dso_local global i32 "++show e_int++"\n"++remaining_codes


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
  let blk_tab = blkTab st
      cur_blk_name = curBlkName st
      new_blk_name = newBlockName cur_blk_name
      new_blk_tab = Map.insert
        new_blk_name
        (BlockState { blkSym = blk_sym
                    , blkVarTab = Map.empty
                    , blkConstVarTab = Map.empty
                    , closureBlkNames = newClosureBlockNames cur_blk_name blk_tab})
        blk_tab
  modify (\st -> st {curBlkName = new_blk_name, blkTab = new_blk_tab})

  items' <- blockItems items

  modify (\st -> st {curBlkName = cur_blk_name})

  return $ foldl1 (++) items'

  where newClosureBlockNames cur_blk_name bs = cur_blk_name:(closureBlkNames (bs Map.! cur_blk_name))
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
  st <- get
  case lookupConstVar id st of Nothing -> return ""
                               Just _ -> error $ "const variable "++id++" already exists"
  let e_int = case evalConstExp e st of Right r -> r
                                        Left l -> error $ show l++"\n    in "++show e
  if e_int == e_int  -- force evaluating the thunk e_int
  then do modify (insertConstVar id e_int)
          decl (ConstDecl BInt cds)
  else return ""


decl (VarDecl BInt []) = return ""

decl (VarDecl BInt ((VarDef1 id):vds)) = do
  st <- get
  case lookupVar id st of Nothing -> return ""
                          Just _ -> error $ "variable "++id++" already exists"
  let sym_tab = symTab st
      new_sym = newSym sym_tab
  modify (\st -> st {symTab = appendSym id new_sym sym_tab})
  modify (insertVar id new_sym)
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
  st <- get
  let lvar_sym = case lookupVarRecur id st of Just j -> j
                                              Nothing -> error $ "LVal "++id++" not found"
  return $ exp_codes++"    store i32 "++lastSym (symTab st)++", i32* "++lvar_sym++"\n"

stmt (Stmt2 e) = expr e

stmt StmtSemiColon = return ""

stmt (StmtReturn e) = do
  exp_codes <- expr e
  sym_tab <- gets symTab
  modify (\st -> st {symTab = ("", newSym sym_tab):sym_tab})
  return $ exp_codes++"    ret i32 "++lastSym sym_tab++"\n"

stmt (StmtIfElse c stmt_if stmt_else) = do
  cond_codes <- cond c
  sym_tab <- gets symTab
  case stmt_else of
    StmtSemiColon -> do
      sym_tab <- gets symTab

      let if_label = newSym sym_tab
          cond_sym = lastSym sym_tab
      modify (\st -> st {symTab = appendSym "" if_label sym_tab})

      s1_codes <- stmt stmt_if
      sym_tab_if <- gets symTab

      let end_label = newSym sym_tab_if
      modify (\st -> st {symTab = appendSym "" end_label sym_tab_if})

      let br_if_code = "    br i1 "++cond_sym++", label "++if_label++", label "++end_label++"\n"
          br_end_code = "    br label "++end_label++"\n"
      return $ cond_codes++br_if_code++labelCode if_label++s1_codes++br_end_code++labelCode end_label
    _ -> do
      sym_tab <- gets symTab

      let if_label = newSym sym_tab
          cond_sym = lastSym sym_tab
      modify (\st -> st {symTab = appendSym "" if_label sym_tab})

      if_codes <- stmt stmt_if
      sym_tab_if <- gets symTab

      let else_label = newSym sym_tab_if
      modify (\st -> st {symTab = appendSym "" else_label sym_tab_if})

      else_codes <- stmt stmt_else
      sym_tab_else <- gets symTab

      let end_label = newSym sym_tab_else
      modify (\st -> st {symTab = appendSym "" end_label sym_tab_else})

      let br_if_code = "    br i1 "++cond_sym++", label "++if_label++", label "++else_label++"\n"
          br_end_code = "    br label "++end_label++"\n"
      return $ cond_codes++br_if_code
        ++labelCode if_label++if_codes++br_end_code
        ++labelCode else_label++else_codes++br_end_code
        ++labelCode end_label
  where labelCode label = "\n"++tail label++":\n"

stmt (StmtBlock b) = block b ""

-- Evaluate const expressions
evalConstExp e const_tab = evalConstAddExp e const_tab

evalConstAddExp :: AddExp -> CodegenState -> Either String Int
evalConstAddExp (AddExp1 m) const_tab = evalConstMulExp m const_tab
evalConstAddExp (AddExp2 m op a) const_tab = liftM2 (evalConstOp op) (evalConstMulExp m const_tab) (evalConstAddExp a const_tab)

evalConstMulExp :: MulExp -> CodegenState -> Either String Int
evalConstMulExp (MulExp1 u) const_tab = evalConstUnaryExp u const_tab
evalConstMulExp (MulExp2 u op m) const_tab = liftM2 (evalConstOp op) (evalConstUnaryExp u const_tab) (evalConstMulExp m const_tab)

evalConstUnaryExp :: UnaryExp -> CodegenState -> Either String Int
evalConstUnaryExp (UnaryExp1 p) const_tab = evalConstPrimaryExp p const_tab
evalConstUnaryExp (UnaryExp2 op u) const_tab = liftM (evalConstOp op 0) (evalConstUnaryExp u const_tab)
evalConstUnaryExp (UnaryExpCallFunc i _) const_tab = Left "using function call in a const context"

evalConstPrimaryExp :: PrimaryExp -> CodegenState -> Either String Int
evalConstPrimaryExp (PrimaryExp1 e) const_tab = evalConstExp e const_tab
evalConstPrimaryExp (PrimaryExp2 n) const_tab = return n
evalConstPrimaryExp (PrimaryExp3 (LVal id)) const_tab =
  case lookupConstVarRecur id const_tab of Just v -> Right v
                                           Nothing -> Left "using non-const variable in a const context"

-- Expressions

unaryOpCodes :: UnaryOp -> String -> Codegen String
unaryOpCodes LNot operand1 = do
  sym_tab <- gets symTab
  let new_sym = newSym sym_tab
      new_sym_zext = newSym' new_sym
  modify (\st -> st {symTab = appendSym "" new_sym sym_tab})
  modify (\st -> st {symTab = appendSym "" new_sym_zext sym_tab})
  return $ "    "++new_sym++" = icmp eq i32 0, "++operand1++"\n"
    ++"    "++new_sym_zext++" = zext i1 "++new_sym++" to i32\n"
unaryOpCodes op operand1 = opCodes op "0" operand1
  
opCodes :: (OpCode a) => a -> String -> String -> Codegen String
opCodes op operand1 operand2 = do
  sym_tab <- gets symTab
  let new_sym = newSym sym_tab
      new_code = tripleCode (opInstr op) new_sym operand1 operand2
  modify (\st -> st {symTab = appendSym "" new_sym sym_tab})
  return new_code

expr :: Exp -> Codegen String
expr e = addExp e


addExp :: AddExp -> Codegen String

addExp (AddExp1 m) = mulExp m

addExp (AddExp2 m1 op a2)
  | isNum m1 && isNum a2 = opCodes op (show . getNum $ m1) (show . getNum $ a2)
  | isNum m1 = do
    a2_codes <- addExp a2
    a2_sym <- gets (lastSym . symTab)
    new_code <- opCodes op (show . getNum $ m1) a2_sym
    return $ a2_codes++new_code
  | otherwise = do
    m1_codes <- mulExp m1
    m1_sym <- gets (lastSym . symTab)
    a2_codes <- addExp a2
    a2_sym <- gets (lastSym . symTab)
    new_code <- opCodes op m1_sym a2_sym
    return $ m1_codes++a2_codes++new_code


mulExp :: MulExp -> Codegen String

mulExp (MulExp1 u) = unaryExp u

mulExp (MulExp2 u1 op m2)
  | isNum u1 = do
    m2_codes <- mulExp m2
    m2_sym <- gets (lastSym . symTab)
    new_code <- opCodes op (show . getNum $ u1) m2_sym
    return $ m2_codes++new_code
  | otherwise = do
    u1_codes <- unaryExp u1
    u1_sym <- gets (lastSym . symTab)
    m2_codes <- mulExp m2
    m2_sym <- gets (lastSym . symTab)
    new_code <- opCodes op u1_sym m2_sym
    return $ u1_codes++m2_codes++new_code


unaryExp :: UnaryExp -> Codegen String

unaryExp (UnaryExp1 (PrimaryExp1 e)) = expr e
unaryExp (UnaryExp1 (PrimaryExp2 n)) = opCodes Pos "0" (show n)
unaryExp (UnaryExp1 (PrimaryExp3 (LVal id))) = do
  st <- get
  sym_tab <- gets symTab
  let lvar_sym = lookupVarRecur id st
      const_var = lookupConstVarRecur id st
      new_sym = newSym sym_tab
  modify (\st -> st {symTab = appendSym "" new_sym sym_tab})
  case lvar_sym of Just j -> return $ "    "++new_sym++" = load i32, i32* "++j++"\n"
                   Nothing -> case const_var of Just cv -> return $ "    "++new_sym++" = add i32 0, "++show cv++"\n"
                                                Nothing -> error $ "LVal "++id++" is neither variable or const variable, st = "++show st

unaryExp (UnaryExp2 op (UnaryExp1 p)) =
  case p of (PrimaryExp1 e) -> do
              e_codes <- expr e
              e_sym <- gets (lastSym . symTab)
              new_code <- unaryOpCodes op e_sym
              return $ e_codes++new_code
            (PrimaryExp2 n) -> unaryOpCodes op (show n)
            (PrimaryExp3 (LVal id)) -> do
              u_codes <- unaryExp (UnaryExp1 p)
              u_sym <- gets (lastSym . symTab)
              new_code <- unaryOpCodes op u_sym
              return $ u_codes++new_code


unaryExp (UnaryExp2 op u) = do
  u_codes <- unaryExp u
  u_sym <- gets (lastSym . symTab)
  new_code <- unaryOpCodes op u_sym
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

--------------------------------- cond -----------------------------------------
cond c = lorExp c

lorExp (LOrExp1 a) = landExp a
lorExp (LOrExp2 a o) = do
  a_codes <- landExp a
  a_sym <- gets (lastSym . symTab)
  o_codes <- lorExp o
  o_sym <- gets (lastSym . symTab)
  sym_tab <- gets symTab
  let new_sym = newSym sym_tab
  modify (\st -> st {symTab = ("", new_sym):sym_tab})
  return $ a_codes++o_codes++"    "++new_sym++" = or i1 "++a_sym++", "++o_sym++"\n"

landExp (LAndExp1 e) = eqExp e
landExp (LAndExp2 e a) = do
  e_codes <- eqExp e
  e_sym <- gets (lastSym . symTab)
  a_codes <- landExp a
  a_sym <- gets (lastSym . symTab)
  sym_tab <- gets symTab
  let new_sym = newSym sym_tab
  modify (\st -> st {symTab = ("", new_sym):sym_tab})
  return $ e_codes++a_codes++"    "++new_sym++" = and i1 "++e_sym++", "++a_sym++"\n"

eqExp (EqExp1 r) = relExp r
eqExp (EqExp2 (RelExp1 a1) eq (EqExp1 (RelExp1 a2))) = relExp (RelExp2 a1 eq (RelExp1 a2))

relExp (RelExp1 a) = do
  a_codes <- addExp a
  a_sym <- gets (lastSym . symTab)
  sym_tab <- gets symTab
  let new_sym = newSym sym_tab
  modify (\st -> st {symTab = appendSym "" new_sym sym_tab})
  return $ a_codes
    ++"    "++new_sym++" = icmp ne i32 0, "++a_sym++"\n"
relExp (RelExp2 a1 cmp (RelExp1 a2)) = do
  a1_codes <- addExp a1
  a1_sym <- gets (lastSym . symTab)
  a2_codes <- addExp a2
  a2_sym <- gets (lastSym . symTab)
  sym_tab <- gets symTab
  let new_sym = newSym sym_tab
  modify (\st -> st {symTab = ("", new_sym):sym_tab})
  return $ a1_codes++a2_codes++"    "++new_sym++" = icmp "++cmpInstr cmp++" i32 "++a1_sym++", "++a2_sym++"\n"
