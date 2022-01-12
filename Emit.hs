module Emit where

import Syntax

type SymbolTable = [String]

emit :: CompUnit -> Either String String
emit (CompUnit (FuncDef TypeInt Main (Block (Return expr)))) = do
  codeExp <- codegenExp [] expr 
  return $ "define dso_local i32 @main() {\n" ++ snd codeExp
    ++ "    ret i32 " ++ head (fst codeExp) ++ "\n}"

newSym sym = "%" ++ show ((read (tail sym)::Int) + 1)

opCode1 Pos = "add"
opCode1 Neg = "sub"
opCode2 Mul = "mul"
opCode2 Div = "sdiv"
opCode2 Mod = "srem"

tripleCode :: String -> String -> String -> String -> String
tripleCode op dest src1 src2  = "    " ++ dest ++ " = " ++ op ++ " i32 "
  ++ src1 ++ ", " ++ src2 ++ "\n"

codegenExp :: SymbolTable -> Exp -> Either String (SymbolTable, String)
codegenExp st (Exp a) = codegenAddExp st a


codegenAddExp :: SymbolTable -> AddExp -> Either String (SymbolTable, String)

codegenAddExp st (AddExp1 m) = codegenMulExp st m

codegenAddExp st (AddExp2 (MulExp1 (UnaryExp1 (PrimaryExp2 a))) op (AddExp1 (MulExp1 (UnaryExp1 (PrimaryExp2 b))))) = do
  let sym_new = if st == [] then "%1" else newSym (head st)
      code_new = tripleCode (opCode1 op) sym_new (show a)  (show b)
   in return (sym_new:st, code_new)

codegenAddExp st (AddExp2 (MulExp1 (UnaryExp1 (PrimaryExp2 x))) op a) = do
  a_ret <- codegenAddExp st a
  let sym1 = head (fst a_ret)
      sym_new = newSym sym1
      code_new = tripleCode (opCode1 op) sym_new sym1 (show x)
   in return (sym_new:fst a_ret, snd a_ret ++ code_new)

codegenAddExp st (AddExp2 m op a) = do
  a_ret <- codegenAddExp st a
  m_ret <- codegenMulExp (fst a_ret) m
  let sym1 = head (fst a_ret)
      sym2 = head (fst m_ret)
      sym_new = newSym sym2
      code_new = tripleCode (opCode1 op) sym_new sym1 sym2
   in return (sym_new:fst m_ret, snd a_ret ++ snd m_ret ++ code_new)


codegenMulExp :: SymbolTable -> MulExp -> (Either String (SymbolTable, String))

codegenMulExp st (MulExp1 u) = codegenUnaryExp st u

codegenMulExp st (MulExp2 (UnaryExp1 (PrimaryExp2 x)) op m) = do
  m_ret <- codegenMulExp st m
  let sym1 = head (fst m_ret)
      sym_new = newSym sym1
      code_new = tripleCode (opCode2 op) sym_new (show x) sym1
   in return (sym_new:fst m_ret, snd m_ret ++ code_new)

codegenMulExp st (MulExp2 u op m) = do
  m_ret <- codegenMulExp st m
  u_ret <- codegenUnaryExp (fst m_ret) u
  let sym1 = head (fst m_ret)
      sym2 = head (fst u_ret)
      sym_new = newSym sym2
      code_new = tripleCode (opCode2 op) sym_new sym1 sym2
   in return (sym_new:fst u_ret, snd m_ret ++ snd u_ret ++ code_new)


codegenUnaryExp :: SymbolTable -> UnaryExp -> (Either String (SymbolTable, String))

codegenUnaryExp st (UnaryExp1 (PrimaryExp1 expr)) = codegenExp st expr

codegenUnaryExp st (UnaryExp2 op (UnaryExp1 (PrimaryExp2 x))) = do
  let sym_new = if st == [] then "%1" else newSym (head st)
      code_new = tripleCode (opCode1 op) sym_new "0" (show x)
   in return (sym_new:st, code_new)

codegenUnaryExp st (UnaryExp2 op u) = do
  u_ret <- codegenUnaryExp st u
  let sym1 = head (fst u_ret)
      sym_new = newSym sym1
      code_new = tripleCode (opCode1 op) sym_new "0" sym1
   in return (sym_new:fst u_ret, snd u_ret ++ code_new)

codegenUnaryExp st u = Left ("pattern matching not exhausted:" ++ show u)
