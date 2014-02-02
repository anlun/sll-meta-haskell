module TreeInterpreter where

import Data
import Data.List
import DataUtil
import Data.Maybe

-- is able to interprete a tree of configuration, given an environment
intTree :: Tree Expr -> Env -> Expr
intTree (Leaf e) env =
    e // env
intTree (Node (Ctr cname _) (EDecompose name ts)) env =
    Ctr name $ map (\t -> intTree t env) ts
intTree (Node _ (ETransient _ t)) env =
    intTree t env
intTree (Node e (EVariants cs)) env =
    head $ catMaybes $ map (try env) cs

try :: Env -> (Subst Expr, Tree Expr) -> (Maybe Expr)
try env ([(v, ctr@(Ctr pn _))], t) =
    if cn == pn then (Just $ intTree t extEnv) else Nothing where
        c@(Ctr cn cargs) = (Var v []) // env
        extEnv = zip (vnames ctr) cargs ++ env
