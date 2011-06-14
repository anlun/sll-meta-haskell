module Driving where

import Data
import DataUtil
import Interpreter

buildConfTree :: Machine Conf -> Conf -> Tree Conf
buildConfTree m c = case m c of
    Stop e -> Leaf e
    Transient test e -> Node c (ETransient test (buildConfTree m e))
    Decompose comp ds -> Node c (EDecompose comp (map (buildConfTree m) ds))
    Variants cs -> Node c (EVariants [(c, buildConfTree m e) | (c, e) <- cs])

confMachine :: Program -> Machine Conf
confMachine p = step where
    step :: Machine Conf
    step e@(Var _ _) = 
        Stop e
    step (GCall gn args) | isVar (head args) = 
        Variants (map (scrutinize args) (gDefs p gn))
    step (GCall gn (arg:args)) | reducible arg , Variants cs <- step arg = 
        Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)
    step (TestEq (e1, e2) bs) | reducible e1, Variants cs <- step e1 = 
        Variants (map (\(c, e1') -> (c, (TestEq (e1', e2) bs))) cs)
    step (TestEq (e1, e2) bs) | reducible e2, Variants cs <- step e2 =
        Variants (map (\(c, e2') -> (c, (TestEq (e1, e2') bs))) cs)
    step (TestEq cond (e1, e2)) | Right (s1, s2) <- test cond = 
        Variants [(s1, e1 // s1), (s2, e2 // s2)]
    step e = 
        exprMachine p e
        
perfectDriveMachine :: Program -> Machine Conf
perfectDriveMachine  = (propagateContraction .) . confMachine

scrutinize ::  [Expr] -> GDef -> (Subst Expr, Expr)
scrutinize ((Var v _) : args) (GDef _ pat@(Pat cn cvs) vs body) = 
    ([(v, Ctr cn fresh)], body // sub) where
        fresh =  makeFreshVars v pat
        sub = zip (cvs ++ vs) (fresh ++ args)

makeFreshVars :: Name -> Pat -> [Expr]
makeFreshVars n (Pat _ vs) = [Var (show i ++ [delim] ++ n) [] | i <- [1 .. length vs]]

propagateContraction :: Step Conf -> Step Conf
propagateContraction (Variants vs) = Variants [(c, e // c) | (c, e) <- vs]
propagateContraction step = step