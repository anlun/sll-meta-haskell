module SS_CBV_Interpreter (int, intC) where

import Data
import DataUtil

int :: Program -> Expr -> Expr
int p = fst . (intC p)

intC :: Program -> Expr -> (Expr, Integer)
intC p e = until (isValue . fst) (updateState (intStepC p)) (e, 0)

intStepC :: Program -> Expr -> (Expr, Integer)
intStepC p (Ctr name args) =
  let (values, x:xs) = span isValue args
      (steppedX, c) = intStepC p x
  in (Ctr name (values ++ (steppedX:xs)), c)

intStepC p (FCall name args) | all isValue args = (body // zip vs args, 1) where
                                                  (FDef _ vs body) = fDef p name
intStepC p (FCall name args) = (FCall name (values ++ (steppedX:xs)), c) where
                                (values, x:xs) = span isValue args
                                (steppedX, c) = intStepC p x

intStepC p (GCall gname (Ctr cname cargs : args)) | all isValue (cargs ++ args) =
	(body // zip (cvs ++ vs) (cargs ++ args), 1) where
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStepC p (GCall gname args) =
  let (values, x:xs) = span isValue args
      (steppedX, c) = intStepC p x
  in (GCall gname (values ++ (steppedX:xs)), c)

intStepC p (TestEq (el, er) (tb, eb)) | isValue el && isValue er = if el == er then (tb, 0) else (eb, 0)
                                      | isValue el = let (steppedER, c) = intStepC p er
                                                     in (TestEq (el, steppedER) (tb, eb), c)
                                      | otherwise  = let (steppedEL, c) = intStepC p el
                                                     in (TestEq (steppedEL, er) (tb, eb), c)
