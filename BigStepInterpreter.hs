module BigStepInterpreter (int) where

import Data
import DataUtil

int :: Program -> Expr -> Expr
int p (Ctr   name args) = Ctr name $ map (int p) args
int p (FCall name args) =
  int p $ (//) body $ zip vs $ map (int p) args where
    (FDef _ vs body) = fDef p name
int p (GCall gname params) =
  int p $ body // zip (cvs ++ vs) (cargs ++ args) where
    (Ctr cname cargs : args)     = map (int p) params
    (GDef _ (Pat _ cvs) vs body) = gDef p gname cname
int p e = e
