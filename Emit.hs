module Emit where

import Syntax

emit::CompUnit -> Either String String
emit (CompUnit (FuncDef _ _ (Block (Return ret)))) = Right $ "define dso_local i32 @main(){\n    ret i32 "++show ret++"\n}"

