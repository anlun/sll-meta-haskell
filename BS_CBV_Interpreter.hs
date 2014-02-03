--{-# LANGUAGE TemplateHaskell #-}
  
module BS_CBV_Interpreter (int) where

--import Control.DeepSeq.TH
import Data
import DataUtil

--deriveNFData ''Expr

traverse :: Expr -> Bool
traverse e =
  case e of
    Ctr s l -> and $ map traverse l
    otherwise -> False

int :: Program -> Expr -> Expr
int p (Ctr name args) =
  case and (map traverse newArgs) of
    True      -> Ctr name newArgs
    otherwise -> Atom '!'
  where
    newArgs = map (int p) args
int p (FCall name args) =
  case and (map traverse newArgs) of
    True      -> int p $ (//) body $ zip vs newArgs
    otherwise -> Atom '!'
  where
    (FDef _ vs body) = fDef p name
    newArgs = map (int p) args
int p (GCall gname params) =
  case and (map traverse $ cargs ++ args) of
    True -> int p $ body // zip (cvs ++ vs) (cargs ++ args)
    otherwise -> Atom '!'
  where
    (Ctr cname cargs : args)     = map (int p) params
    (GDef _ (Pat _ cvs) vs body) = gDef p gname cname
int p (TestEq (el, er) (tb, fb)) = if int p el == int p er then int p tb else int p fb
int p e = e
