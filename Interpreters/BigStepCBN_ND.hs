module Interpreters.BigStepCBN_ND (int, cross) where

import Data
import DataUtil
import DataIO
import Data.Maybe

cross :: [[a]] -> [[a]]
cross []  = []
cross [x] = map (:[]) x
cross (x:xs) = [z:y | z <- x, y <- cross xs]

int :: Program -> Expr -> [Expr]
int p (Atom a) = [Atom a]

int p c@(Ctr _ []) = [c]
int p (Ctr name args) = [Ctr name a | a <- cross $ map (int p) args]

int p (FCall name args) = concat $
  [int p (body // zip vs args) | (FDef _ vs body) <- fDefs p name, length args == length vs]

int p (GCall gname (arg0:args)) = concat $
  [int p (body // zip (cvs ++ vs) (cargs ++ args)) |
    Ctr cname cargs <- int p arg0
  , GDef _ (Pat _ cvs) vs body <- gDefsPat p gname cname 
  , length cvs + length vs == length cargs + length args]

int p (TestEq (x1, x2) (e1, e2)) = concat $
  [if a1==a2 then int p e1 else int p e2 | Atom a1 <- int p x1, Atom a2 <- int p x2]

int _ _ = []
