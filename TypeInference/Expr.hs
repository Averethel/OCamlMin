{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Expr (typeOfExpr) where
  import Syntax.Expr
  import Types

  import TypeInference.BinaryPrim
  import TypeInference.Constant
  import TypeInference.Constraints
  import TypeInference.Counter
  import TypeInference.Env
  import TypeInference.Pattern
  import TypeInference.UnaryPrim
  import TypeInference.Unification

  import Control.Monad.Error
  import Control.Monad.State hiding (get)

  typeOfFunClause :: (MonadState Counter m, MonadError String m) =>
                     Env -> Constraints -> FunClause -> m (Type, Subst)
  typeOfFunClause env cns fc = do
    tbcns  <- mapM typeAndBindingsOfPattern $ arguments fc
    let (tas, bas, cas) = unzip3 tbcns
    (t, s) <- typeOfExpr (concat bas ++ env) (concat cas ++ cns) $ body fc
    s'     <- unify (concat cas ++ cns)
    return (Tfun tas t `applySubst` s `applySubst` s', s ++ s')

  typeOfFunction :: (MonadState Counter m, MonadError String m) =>
                    Env -> Constraints -> [FunClause] -> m (Type, Subst)
  typeOfFunction env cns fcs = do
    tfcs <- mapM (typeOfFunClause env cns) fcs
    let (t:tps, ss) = unzip tfcs
    s <- unify $ cns ++ map ((,) t) tps
    let subst = foldl (++) s ss
    return (t `applySubst` subst, subst)


  typeOfExpr :: (MonadState Counter m, MonadError String m) =>
                Env -> Constraints -> Expr -> m (Type, Subst)
  typeOfExpr _   cns (Econst c)         = do
    t <- typeOfConstant c
    s <- unify cns
    return (t `applySubst` s, s)
  typeOfExpr _   cns (Euprim up)        = do
    t <- typeOfUnaryPrim up
    s <- unify cns
    return (t `applySubst` s, s)
  typeOfExpr _   cns (Ebprim bp)        = do
    t <- typeOfBinaryPrim bp
    s <- unify cns
    return (t `applySubst` s, s)
  typeOfExpr env cns (Evar n)           = do
    t <- n `get` env
    s <- unify cns
    return (t `applySubst` s, s)
  typeOfExpr env cns (Efun fcs)         =
    typeOfFunction env cns fcs
  typeOfExpr env cns (Elet p e1 e2)     = do
    (tp, bs, cs) <- typeAndBindingsOfPattern p
    (t1, s1)     <- typeOfExpr env cns e1
    let cns' = singleConstraint t1 tp `addConstraints` cs `addConstraints` cns
    let env' = map (\(n, t) -> (n, t `applySubst` s1)) bs ++ env
    typeOfExpr env' cns' e2
  typeOfExpr env cns (Eletrec n fcs e2) = do
    v        <- freshVar
    (t1, s1) <- typeOfFunction (env `extend` (n, v)) cns fcs
    let cns' = singleConstraint v t1 `addConstraints` cns
    s        <- unify cns'
    typeOfExpr (env `extend` (n, t1 `applySubst` s1 `applySubst` s)) cns' e2
  typeOfExpr env cns (Eapply e1 as)     = do
    (t1, s1) <- typeOfExpr env cns e1
    tas      <- mapM (typeOfExpr env cns) as
    let (ts, _) = unzip tas
    v        <- freshVar
    s        <- unify $ singleConstraint t1 (Tfun ts v) `addConstraints` cns
    return (v `applySubst` s `applySubst` s1, s)
  typeOfExpr env cns (Epair e1 e2)      = do
    (t1, _) <- typeOfExpr env cns e1
    (t2, _) <- typeOfExpr env cns e2
    s       <- unify cns
    return (Tpair t1 t2 `applySubst` s, s)
  typeOfExpr env cns (Econs e1 e2)      = do
    (t1, s1) <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    s        <- unify $ singleConstraint t2 (Tlist t1) `addConstraints` cns
    return (t2 `applySubst` s `applySubst` s1 `applySubst` s2, s ++ s1 ++ s2)
  typeOfExpr env cns (Eif e1 e2 e3)     = do
    (t1, _)  <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    (t3, s3) <- typeOfExpr env cns e3
    s        <- unify $ singleConstraint t2 t3 `addConstraints`
                        singleConstraint t1 Tbool `addConstraints`
                        cns
    return (t2 `applySubst` s `applySubst` s2 `applySubst` s3, s ++ s2 ++ s3)
  typeOfExpr env cns (Eseq e1 e2)       = do
    (t1, _)  <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    s        <- unify $ singleConstraint t1 Tunit `addConstraints` cns
    return (t2 `applySubst` s, s ++ s2)
  typeOfExpr env cns (Ehandle e1 e2)    = do
    (t1, _)  <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    s        <- unify $ singleConstraint t1 t2 `addConstraints` cns
    return (t2 `applySubst` s, s ++ s2)
  typeOfExpr _   cns EmatchFailure      = do
    v <- freshVar
    s <- unify cns
    return (v, s)
