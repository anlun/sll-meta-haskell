module Interpreters.BigStepCBN_Maybe (int) where

import Data
import DataUtil
import DataIO
import Data.Maybe

int :: Program -> Expr -> Maybe Expr
int p (Atom a) = Just $ Atom a

int p (Ctr name args) | l <- map (int p) args, all isJust l = Just $ Ctr name $ catMaybes l

int p (FCall name args) | ((FDef _ vs body):_) <- fDefs p name, length args == length vs =
                          	int p (body // zip vs args)

int p (GCall gname (arg0:args)) | Just (Ctr cname cargs) <- int p arg0
                                , ((GDef _ (Pat _ cvs) vs body):_) <- gDefsPat p gname cname 
                                , length cvs + length vs == length cargs + length args =
                                	int p (body // zip (cvs ++ vs) (cargs ++ args))

int p (TestEq (x1, x2) (e1, e2)) | Just (Atom a1) <- int p x1, Just (Atom a2) <- int p x2 =
	if a1==a2 then int p e1 else int p e2 

int _ _ = Nothing 
