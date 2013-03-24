{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.NumbersToIfs (numbersToIfs) where
  import Syntax
  import TypedSyntax
  import Types

  import PatternMatching.Counters

  import Control.Monad.State

  mkAnd :: TypedExpr -> TypedExpr -> TypedExpr
  mkAnd (TEconst (Cbool True, _)) e = e
  mkAnd e (TEconst (Cbool True, _)) = e
  mkAnd e1 e2 =
    TEapply (TEbprim (BPand, Tfun [Tbool, Tbool] Tbool)) [e1, e2] Tbool

  numbersToIfsPattern :: MonadState Counter m => TypedPattern ->
                         m (TypedPattern, TypedExpr)
  numbersToIfsPattern (TPconst n@(Cint _, t)) = do
    a <- freshArg t
    return (TPvar a Tint, TEapply (TEbprim (BPeq, Tfun [Tint, Tint] Tbool))
            [TEvar a Tint, TEconst n] Tbool)
  numbersToIfsPattern (TPpair p1 p2 t)       = do
    (p1', a1') <- numbersToIfsPattern p1
    (p2', a2') <- numbersToIfsPattern p2
    return (TPpair p1' p2' t, mkAnd a1' a2')
  numbersToIfsPattern (TPcons p1 p2 t)       = do
    (p1', a1') <- numbersToIfsPattern p1
    (p2', a2') <- numbersToIfsPattern p2
    return (TPcons p1' p2' t, mkAnd a1' a2')
  numbersToIfsPattern p                      =
    return (p, TEconst (Cbool True, Tbool))

  numbersToIfsPatterns :: MonadState Counter m => [TypedPattern] ->
                          m ([TypedPattern], TypedExpr)
  numbersToIfsPatterns ps = do
    pscs <- mapM numbersToIfsPattern ps
    let (ps', cs) = unzip pscs
    return (ps', foldl mkAnd (TEconst (Cbool True, Tbool)) cs)

  numbersToIfsCaseClause :: MonadState Counter m => TypedCaseClause ->
                            m TypedCaseClause
  numbersToIfsCaseClause cc = do
    b' <- numbersToIfs $ tccBody cc
    return cc{ tccBody = b' }

  numbersToIfsFunClause :: MonadState Counter m => TypedFunClause ->
                           m TypedFunClause
  numbersToIfsFunClause fc = do
    (ps, c) <- numbersToIfsPatterns $ tfcArguments fc
    b'      <- numbersToIfs $ tfcBody fc
    case c of
      TEconst (Cbool True, _) -> return fc { tfcBody = b' }
      _                       -> do
        let t = typeOfTypedExpr b'
        return fc { tfcArguments = ps,
                    tfcBody = TEif c b' (TEmatchFailure t) t }

  numbersToIfs :: MonadState Counter m => TypedExpr -> m TypedExpr
  numbersToIfs (TEfun fcs t)            = do
    fcs' <- mapM numbersToIfsFunClause fcs
    return $ TEfun fcs' t
  numbersToIfs (TElet p e1 e2 t)        = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ TElet p e1' e2' t
  numbersToIfs (TEletrec n t1 fcs e t2) = do
    fcs' <- mapM numbersToIfsFunClause fcs
    e'   <- numbersToIfs e
    return $ TEletrec n t1 fcs' e' t2
  numbersToIfs (TEapply e1 as t)        = do
    e1' <- numbersToIfs e1
    as' <- mapM numbersToIfs as
    return $ TEapply e1' as' t
  numbersToIfs (TEpair e1 e2 t)         = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ TEpair e1' e2' t
  numbersToIfs (TEcons e1 e2 t)         = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ TEcons e1' e2' t
  numbersToIfs (TEif e1 e2 e3 t)        = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    e3' <- numbersToIfs e3
    return $ TEif e1' e2' e3' t
  numbersToIfs (TEseq e1 e2 t)          = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ TEseq e1' e2' t
  numbersToIfs (TEcase e ccs t)         = do
    e'   <- numbersToIfs e
    ccs' <- mapM numbersToIfsCaseClause ccs
    return $ TEcase e' ccs' t
  numbersToIfs (TEhandle e1 e2 t)       = do
    e1' <- numbersToIfs e1
    e2' <- numbersToIfs e2
    return $ TEhandle e1' e2' t
  numbersToIfs e                        =
    return e
