{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Expr (typeOfExpr) where
  import Counters
  import Syntax.BinaryPrim
  import Syntax.Expr
  import TypedSyntax.Expr
  import TypedSyntax.Pattern
  import Types

  import TypeInference.BinaryPrim
  import TypeInference.Constant
  import TypeInference.Constraints
  import TypeInference.Constructor
  import TypeInference.Env
  import TypeInference.Pattern
  import TypeInference.UnaryPrim
  import TypeInference.Unification

  import Utils.Errors

  import Control.Monad.Error
  import Control.Monad.State hiding (get)

  typeOfFunClause :: (MonadState Counter m, MonadError String m) =>
                     Env -> Constraints -> FunClause ->
                     m (TypedFunClause, Subst)
  typeOfFunClause env cns fc = do
    tbcns  <- mapM typeAndBindingsOfPattern $ arguments fc
    let (tas, bas, cas) = unzip3 tbcns
    (te, s) <- typeOfExpr (concat bas ++ env) (concat cas ++ cns) $ fbody fc
    s'      <- unify (concat cas ++ cns)
    return (TFC tas te `applySubstTFC` s `applySubstTFC` s', s ++ s')

  typeOfFunction :: (MonadState Counter m, MonadError String m) =>
                    Env -> Constraints -> [FunClause] ->
                    m ([TypedFunClause], Type, Subst)
  typeOfFunction env cns fcs = do
    tfcs <- mapM (typeOfFunClause env cns) fcs
    let (fcs', ss) = unzip tfcs
    let (t:tps) = map typeOfTypedFunClause fcs'
    s <- unify $ cns ++ map ((,) t) tps
    let subst = foldl (++) s ss
    return (map (`applySubstTFC` subst) fcs', t `applySubst` subst, subst)

  typeOfCaseClause :: (MonadState Counter m, MonadError String m) =>
                      Env -> Constraints -> CaseClause ->
                      m (TypedCaseClause, Subst)
  typeOfCaseClause env cns cc = do
    (tp, bs) <- typeAndBindingsOfConstructor (constructor cc) $ variables cc
    (te, s)  <- typeOfExpr (bs ++ env) cns $ cbody cc
    s'       <- unify cns
    return (TCC tp bs te `applySubstTCC` s' `applySubstTCC` s, s' ++ s)

  typeOfCase :: (MonadState Counter m, MonadError String m) =>
                Env -> Constraints -> [CaseClause] ->
                m ([TypedCaseClause], Subst)
  typeOfCase env cns cls = do
    tcls <- mapM (typeOfCaseClause env cns) cls
    let (cs, ss) = unzip tcls
    let (t:tps)  = map typeOfTypedCaseClause cs
    s <- unify $ cns `addConstraints` map ((,) t) tps
    let subst = foldl (++) s ss
    return (map (`applySubstTCC` subst) cs, subst)


  typeOfExpr :: (MonadState Counter m, MonadError String m) =>
                Env -> Constraints -> Expr -> m (TypedExpr, Subst)
  typeOfExpr _   cns (Econst c)         = do
    t <- typeOfConstant c
    s <- unify cns
    return (TEconst (c, t `applySubst` s), s)
  typeOfExpr _   cns (Euprim up)        = do
    t <- typeOfUnaryPrim up
    s <- unify cns
    return (TEuprim (up, t `applySubst` s), s)
  typeOfExpr _   cns (Ebprim bp)        = do
    t <- typeOfBinaryPrim bp
    s <- unify cns
    return (TEbprim (bp, t `applySubst` s), s)
  typeOfExpr env cns (Evar n)           = do
    t <- n `get` env
    s <- unify cns
    return (TEvar n $ t `applySubst` s, s)
  typeOfExpr env cns (Efun fcs)         = do
    (fcs', t, s) <- typeOfFunction env cns fcs
    return (TEfun fcs' t, s)
  typeOfExpr env cns (Elet p e1 e2)     = do
    (tp, bs, cs) <- typeAndBindingsOfPattern p
    (t1, s1)     <- typeOfExpr env cns e1
    let cns' = singleConstraint (typeOfTypedExpr t1) (typeOfTypedPattern tp)
              `addConstraints` cs `addConstraints` cns
    let env' = map (\(n, t) -> (n, t `applySubst` s1)) bs ++ env
    (t2, s2) <- typeOfExpr env' cns' e2
    return (TElet tp t1 t2 $ typeOfTypedExpr t2, s2)
  typeOfExpr env cns (Eletrec n fcs e2) = do
    v              <- freshTypeVar
    (fcs', t1, s1) <- typeOfFunction (env `extend` (n, v)) cns fcs
    let cns'        = singleConstraint v t1 `addConstraints` cns
    s              <- unify cns'
    (t2, s2)       <- typeOfExpr (env `extend` (n, t1 `applySubst` s1
                                      `applySubst` s)) cns' e2
    return (TEletrec n t1 fcs' t2 $ typeOfTypedExpr t2, s2)
  typeOfExpr env cns (Eapply (Ebprim BPeq) as) = do
    tas    <- mapM (typeOfExpr env cns) as
    s      <- unify cns
    let ts  = map (`applySubstTE` s) $ fst $ unzip tas
    case map typeOfTypedExpr ts of
      [Tint, Tint]   -> return $ unifyEq ts Tint s
      [Tbool, Tbool] -> return $ unifyEq ts Tbool s
      l              -> throwError $ equalitySimple $ head l
      where
        unifyEq ts tp s =
          (TEapply (TEbprim (BPeq, Tfun [tp, tp] Tbool)) ts Tbool, s)
  typeOfExpr env cns (Eapply e1 as)     = do
    (t1, s1) <- typeOfExpr env cns e1
    tas      <- mapM (typeOfExpr env cns) as
    let (ts, _) = unzip tas
    v        <- freshTypeVar
    s        <- unify $ singleConstraint (typeOfTypedExpr t1)
                        (Tfun (map typeOfTypedExpr ts) v) `addConstraints` cns
    let e1' = t1 `applySubstTE` s `applySubstTE` s1
    let es' = map (\e -> e `applySubstTE` s `applySubstTE` s1) ts
    return (TEapply e1' es' $  v `applySubst` s `applySubst` s1, s)
  typeOfExpr env cns (Epair e1 e2)      = do
    (t1, _) <- typeOfExpr env cns e1
    (t2, _) <- typeOfExpr env cns e2
    s       <- unify cns
    let e1' = t1 `applySubstTE` s
    let e2' = t2 `applySubstTE` s
    return (TEpair e1' e2' $
            Tpair (typeOfTypedExpr e1') (typeOfTypedExpr e2'), s)
  typeOfExpr env cns (Econs e1 e2)      = do
    (t1, s1) <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    s        <- unify $ singleConstraint (typeOfTypedExpr t2)
                        (Tlist $ typeOfTypedExpr t1) `addConstraints` cns
    let e1' = t1 `applySubstTE` s `applySubstTE` s1 `applySubstTE` s2
    let e2' = t2 `applySubstTE` s `applySubstTE` s1 `applySubstTE` s2
    return (TEcons e1' e2' $ typeOfTypedExpr e2', s ++ s1 ++ s2)
  typeOfExpr env cns (Eif e1 e2 e3)     = do
    (t1, _)  <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    (t3, s3) <- typeOfExpr env cns e3
    s        <- unify $ singleConstraint (typeOfTypedExpr t2)
                        (typeOfTypedExpr t3) `addConstraints` singleConstraint
                        (typeOfTypedExpr t1) Tbool `addConstraints` cns
    let e1' = t1 `applySubstTE` s `applySubstTE` s2 `applySubstTE` s3
    let e2' = t2 `applySubstTE` s `applySubstTE` s2 `applySubstTE` s3
    let e3' = t3 `applySubstTE` s `applySubstTE` s2 `applySubstTE` s3
    return (TEif e1' e2' e3' $ typeOfTypedExpr e2', s ++ s2 ++ s3)
  typeOfExpr env cns (Eseq e1 e2)       = do
    (t1, _)  <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    s        <- unify $ singleConstraint (typeOfTypedExpr t1) Tunit
                        `addConstraints` cns
    let e1' = t1 `applySubstTE` s
    let e2' = t2 `applySubstTE` s
    return (TEseq e1' e2' $ typeOfTypedExpr e2', s ++ s2)
  typeOfExpr env cns (Ecase e1 cls)     = do
    (t1, s1)  <- typeOfExpr env cns e1
    (tcs, s2) <- typeOfCase env cns cls
    let (Tfun [ta] tr) = typeOfTypedCaseClause . head $ tcs
    s         <- unify $ singleConstraint ta (typeOfTypedExpr t1)
                         `addConstraints` cns
    let e1'  = t1 `applySubstTE` s `applySubstTE` s2 `applySubstTE` s1
    let cls' = map (\c -> c `applySubstTCC` s
                            `applySubstTCC` s2 `applySubstTCC` s1) tcs
    return (TEcase e1' cls' $ tr `applySubst` s
                                 `applySubst` s2 `applySubst` s1, s ++ s2 ++ s1)
  typeOfExpr env cns (Ehandle e1 e2)    = do
    (t1, _)  <- typeOfExpr env cns e1
    (t2, s2) <- typeOfExpr env cns e2
    s        <- unify $ singleConstraint (typeOfTypedExpr t1)
                        (typeOfTypedExpr t2) `addConstraints` cns
    let e1' = t1 `applySubstTE` s
    let e2' = t2 `applySubstTE` s
    return (TEhandle e1' e2' $ typeOfTypedExpr e2', s ++ s2)
  typeOfExpr _   cns EmatchFailure      = do
    v <- freshTypeVar
    s <- unify cns
    return (TEmatchFailure v, s)
