module SS_CBV_Interpreter (int) where

import Data
import DataUtil

int :: Program -> Expr -> Expr
int p e = until isValue (intStep p) e

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) =
	Ctr name (values ++ (intStep p x : xs)) where
		(values, x : xs) = span isValue args

intStep p (FCall name args) | all isValue args = body // zip vs args where
                                                  (FDef _ vs body) = fDef p name
intStep p (FCall name args) = FCall name (values ++ (intStep p x : xs)) where
                                (values, x : xs) = span isValue args

intStep p (GCall gname (Ctr cname cargs : args)) | all isValue (cargs ++ args)=
	body // zip (cvs ++ vs) (cargs ++ args) where
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStep p (GCall gname (Ctr cname cargs : args)) | all isValue cargs =
  GCall gname (Ctr cname cargs : (values ++ (intStep p x : xs))) where
    (values, x : xs) = span isValue args

intStep p (GCall gname (Ctr cname cargs : args)) =
  GCall gname (Ctr cname (values ++ (intStep p x : xs)) : args) where
    (values, x : xs) = span isValue cargs

intStep p (GCall gname (e:es)) =
	(GCall gname (intStep p e : es))
