module Emit where

import Syntax

type SymbolTable = [String]

emit :: CompUnit -> Either String String
emit (CompUnit (FuncDef TypeInt Main (Block (Return expr)))) = do
  codeExp <- codegenExp [] expr 
  return $ "define dso_local i32 @main() {\n" ++ snd codeExp ++ "}"

newSym sym = "%" ++ show ((read (tail sym)::Int) + 1)

codegenExp :: SymbolTable -> Exp -> Either String (SymbolTable, String)
codegenExp st (Exp a) = codegenAddExp st a

codegenAddExp :: SymbolTable -> AddExp -> Either String (SymbolTable, String)
codegenAddExp st (AddExp1 m) = codegenMulExp st m
codegenAddExp st (AddExp2 (MulExp1 (UnaryExp1 (PrimaryExp2 a))) op (AddExp1 (MulExp1 (UnaryExp1 (PrimaryExp2 b))))) = do
  let sym_new = if st == [] then "%0" else newSym (head st)
      opcode = if op == Pos then "add" else "sub"
      code_new = "    " ++ sym_new ++ " = " ++ opcode ++ " i32 " ++ show a ++ ", " ++ show b ++ "\n"
   in return (sym_new:st, code_new)
codegenAddExp st (AddExp2 (MulExp1 (UnaryExp1 (PrimaryExp2 x))) op a) = do
  a_ret <- codegenAddExp st a
  let sym1 = head (fst a_ret)
      sym_new = newSym sym1
      opcode = if op == Pos then "add" else "sub"
      code_new = "    " ++ sym_new ++ " = " ++ opcode ++ " i32 " ++ sym1 ++ ", " ++ show x ++ "\n"
   in return (sym_new:fst a_ret, snd a_ret ++ code_new)
codegenAddExp st (AddExp2 m op a) = do
  a_ret <- codegenAddExp st a
  m_ret <- codegenMulExp (fst a_ret) m
  let sym1 = head (fst a_ret)
      sym2 = head (fst m_ret)
      sym_new = newSym sym2
      opcode = if op == Pos then "add" else "sub"
      code_new = "    " ++ sym_new ++ " = " ++ opcode ++ " i32 " ++ sym1 ++ ", " ++ sym2 ++ "\n"
   in return (sym_new:fst m_ret, snd a_ret ++ snd m_ret ++ code_new)

codegenMulExp :: SymbolTable -> MulExp -> (Either String (SymbolTable, String))
codegenMulExp st (MulExp1 u) = codegenUnaryExp st u
codegenMulExp st (MulExp2 (UnaryExp1 (PrimaryExp2 x)) op m) = do
  m_ret <- codegenMulExp st m
  let sym1 = head (fst m_ret)
      sym_new = newSym sym1
      opcode = case op of Mul -> "mul"
                          Div -> "sdiv"
                          Mod -> "ERROR"
      code_new = "    " ++ sym_new ++ " = " ++ opcode ++ " i32 " ++ show x ++ ", " ++ sym1 ++ "\n"
   in return (sym_new:fst m_ret, snd m_ret ++ code_new)
codegenMulExp st (MulExp2 u op m) = do
  m_ret <- codegenMulExp st m
  u_ret <- codegenUnaryExp (fst m_ret) u
  let sym1 = head (fst m_ret)
      sym2 = head (fst u_ret)
      sym_new = newSym sym2
      opcode = case op of Mul -> "mul"
                          Div -> "sdiv"
                          Mod -> "ERROR"
      code_new = "    " ++ sym_new ++ " = " ++ opcode ++ " i32 " ++ sym1 ++ ", " ++ sym2 ++ "\n"
   in return (sym_new:fst u_ret, snd m_ret ++ snd u_ret ++ code_new)

codegenUnaryExp :: SymbolTable -> UnaryExp -> (Either String (SymbolTable, String))
codegenUnaryExp st (UnaryExp1 (PrimaryExp1 expr)) = codegenExp st expr
codegenUnaryExp st (UnaryExp2 op (UnaryExp1 (PrimaryExp2 x))) = do
  let sym_new = if st == [] then "%0" else newSym (head st)
      opcode = if op == Pos then "add" else "sub"
      code_new = "    " ++ sym_new ++ " = " ++ opcode ++ " i32 " ++ "0" ++ ", " ++ show x ++ "\n"
   in return (sym_new:st, code_new)
codegenUnaryExp st (UnaryExp2 op u) = do
  u_ret <- codegenUnaryExp st u
  let sym1 = head (fst u_ret)
      sym_new = newSym sym1
      opcode = if op == Pos then "add" else "sub"
      code_new = "    " ++ sym_new ++ " = " ++ opcode ++ " i32 " ++ "0" ++ ", " ++ sym1 ++ "\n"
   in return (sym_new:fst u_ret, snd u_ret ++ code_new)
