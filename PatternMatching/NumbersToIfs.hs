{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.NumbersToIfs (numbersToIfs) where
  import Syntax

  import PatternMatching.Counters

  import Control.Monad.State

  mkAnd :: Expr -> Expr -> Expr
  mkAnd (Econst (Cbool True)) e = e
  mkAnd e (Econst (Cbool True)) = e
  mkAnd e1 e2 = Eapply (Ebprim BPand) [e1, e2]

  numbersToIfsPattern :: MonadState Counter m => Pattern -> m (Pattern, Expr)
  numbersToIfsPattern (Pconst n@(Cint _)) = do
    a <- freshArg
    return (Pvar a, Eapply (Ebprim BPeq) [Evar a, Econst n])
  numbersToIfsPattern (Ppair p1 p2)       = do
    (p1', a1') <- numbersToIfsPattern p1
    (p2', a2') <- numbersToIfsPattern p2
    return (Ppair p1' p2', mkAnd a1' a2')
  numbersToIfsPattern (Pcons p1 p2)       = do
    (p1', a1') <- numbersToIfsPattern p1
    (p2', a2') <- numbersToIfsPattern p2
    return (Pcons p1' p2', mkAnd a1' a2')
  numbersToIfsPattern p                   =
    return (p, Econst $ Cbool True)

  numbersToIfsPatterns :: MonadState Counter m => [Pattern] -> m ([Pattern], Expr)
  numbersToIfsPatterns ps = do
    pscs <- mapM numbersToIfsPattern ps
    let (ps', cs) = unzip pscs
    return (ps', foldl mkAnd (Econst $ Cbool True) cs)

  numbersToIfsCaseClause :: MonadState Counter m => CaseClause -> m CaseClause
  numbersToIfsCaseClause cc = do
    b' <- numbersToIfs $ cbody cc
    return cc{ cbody = b' }

  numbersToIfsFunClause :: MonadState Counter m => FunClause -> m FunClause
  numbersToIfsFunClause fc = do
    (ps, c) <- numbersToIfsPatterns $ arguments fc
    b'      <- numbersToIfs $ fbody fc
    case c of
      Econst (Cbool True) -> return fc { fbody = b' }
      _                   -> return fc { arguments = ps,
                                         fbody = Eif c b' EmatchFailure }

  numbersToIfs :: MonadState Counter m => Expr -> m Expr
  numbersToIfs (Efun fcs)         = do
    fcs' <- mapM numbersToIfsFunClause fcs
    return $ Efun fcs'
  numbersToIfs (Elet p e1 e2)     = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ Elet p e1' e2'
  numbersToIfs (Eletrec n fcs e1) = do
    fcs' <- mapM numbersToIfsFunClause fcs
    e1'  <- numbersToIfs e1
    return $ Eletrec n fcs' e1'
  numbersToIfs (Eapply e1 as)     = do
    e1' <- numbersToIfs e1
    as' <- mapM numbersToIfs as
    return $ Eapply e1' as'
  numbersToIfs (Epair e1 e2)      = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ Epair e1' e2'
  numbersToIfs (Econs e1 e2)      = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ Econs e1' e2'
  numbersToIfs (Eif e1 e2 e3)     = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    e3' <- numbersToIfs e3
    return $ Eif e1' e2' e3'
  numbersToIfs (Eseq e1 e2)       = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ Eseq e1' e2'
  numbersToIfs (Ecase e ccs)      = do
    e'   <- numbersToIfs e
    ccs' <- mapM numbersToIfsCaseClause ccs
    return $ Ecase e' ccs'
  numbersToIfs (Ehandle e1 e2)    = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ Ehandle e1' e2'
  numbersToIfs e                  =
    return e
