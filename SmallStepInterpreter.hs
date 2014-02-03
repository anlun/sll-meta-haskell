module SmallStepInterpreter (int, intC) where

import Data
import DataUtil

int :: Program -> Expr -> Expr
int p e = until isValue (intStep p) e

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) =
	Ctr name (values ++ (intStep p x : xs)) where
		(values, x : xs) = span isValue args

intStep p (FCall name args) =
	body // zip vs args where
		(FDef _ vs body) = fDef p name

intStep p (GCall gname (Ctr cname cargs : args)) =
	body // zip (cvs ++ vs) (cargs ++ args) where
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStep p (GCall gname (e:es)) =
	(GCall gname (intStep p e : es))

------------------------------

updateState :: (Expr -> (Expr, Integer)) -> (Expr, Integer) -> (Expr, Integer)
updateState f (e, n) =
  (e', n + n') where
    (e', n') = f e

intC :: Program -> Expr -> (Expr, Integer)
intC p e = until (isValue . fst) (updateState (intStepC p)) (e, 0)

intStepC :: Program -> Expr -> (Expr, Integer)
intStepC p (Ctr name args) =
  let (values, x:xs) = span isValue args
      (steppedX, c)  = intStepC p x
  in (Ctr name (values ++ (steppedX : xs)), c)

intStepC p (FCall name args) =
	(body // zip vs args, 1) where
		(FDef _ vs body) = fDef p name

intStepC p (GCall gname (Ctr cname cargs : args)) =
	(body // zip (cvs ++ vs) (cargs ++ args), 1) where
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStepC p (GCall gname (e:es)) =
	(GCall gname (steppedE : es), c) where
    (steppedE, c) = intStepC p e
