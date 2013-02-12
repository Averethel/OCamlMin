{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.EliminateLetSubpatterns (eliminateLetSubPatterns) where
  import Syntax.Expr
  import Syntax.Pattern

  import PatternMatching.Counters

  import Control.Monad.State

  getSubPatterns :: MonadState Counter m =>
                    Pattern -> Expr -> m [(Pattern, Expr)]
  getSubPatterns (Ppair p1 p2) e = getSubPatterns' Ppair p1 p2 e
  getSubPatterns (Pcons p1 p2) e = getSubPatterns' Pcons p1 p2 e
  getSubPatterns p             e = return [(p, e)]

  getSubPatterns' :: MonadState Counter m =>
                     (Pattern -> Pattern -> Pattern) -> Pattern -> Pattern ->
                     Expr -> m [(Pattern, Expr)]
  getSubPatterns' cons p1 p2 e
    | isAtomicPattern p1 &&
      isAtomicPattern p2          = return [(cons p1 p2, e)]
    | not (isAtomicPattern p1) &&
      isAtomicPattern p2          = do
        v  <- freshVar
        ps <- getSubPatterns p1 (Evar v)
        return $ (cons (Pvar v) p2, e) : ps
    | isAtomicPattern p1 &&
      not (isAtomicPattern p2)    = do
        v  <- freshVar
        ps <- getSubPatterns p2 (Evar v)
        return $ (cons p1 (Pvar v), e) : ps
    | otherwise                   = do
      v1  <- freshVar
      v2  <- freshVar
      ps1 <- getSubPatterns p1 (Evar v1)
      ps2 <- getSubPatterns p2 (Evar v2)
      return $ (cons (Pvar v1) (Pvar v2), e) : (ps1 ++ ps2)

  eliminateLetSubPatternsFunClause :: MonadState Counter m =>
                                      FunClause -> m FunClause
  eliminateLetSubPatternsFunClause fc = do
    b' <- eliminateLetSubPatterns $ fbody fc
    return fc{ fbody = b' }

  eliminateLetSubPatternsCaseClause :: MonadState Counter m =>
                                       CaseClause -> m CaseClause
  eliminateLetSubPatternsCaseClause cc = do
    b' <- eliminateLetSubPatterns $ cbody cc
    return cc{ cbody = b' }

  eliminateLetSubPatterns :: MonadState Counter m => Expr -> m Expr
  eliminateLetSubPatterns (Efun fcs)        = do
    fcs' <- mapM eliminateLetSubPatternsFunClause fcs
    return $ Efun fcs'
  eliminateLetSubPatterns (Elet p e1 e2)    = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    if   not (hasSubPatterns p)
    then return $ Elet p e1' e2'
    else do
      ps <- getSubPatterns p e1'
      return $ foldr (\(px, ex) ey -> Elet px ex ey) e2' ps
  eliminateLetSubPatterns (Eletrec n fcs e) = do
    fcs' <- mapM eliminateLetSubPatternsFunClause fcs
    e'   <- eliminateLetSubPatterns e
    return $ Eletrec n fcs' e'
  eliminateLetSubPatterns (Eapply e1 as)    = do
    e1' <- eliminateLetSubPatterns e1
    as' <- mapM eliminateLetSubPatterns as
    return $ Eapply e1' as'
  eliminateLetSubPatterns (Epair e1 e2)     = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ Epair e1' e2'
  eliminateLetSubPatterns (Econs e1 e2)     = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ Econs e1' e2'
  eliminateLetSubPatterns (Eif e1 e2 e3)    = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    e3' <- eliminateLetSubPatterns e3
    return $ Eif e1' e2' e3'
  eliminateLetSubPatterns (Eseq e1 e2)      = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ Eseq e1' e2'
  eliminateLetSubPatterns (Ecase e1 ccs)    = do
    e1'  <- eliminateLetSubPatterns e1
    ccs' <- mapM eliminateLetSubPatternsCaseClause ccs
    return $ Ecase e1' ccs'
  eliminateLetSubPatterns (Ehandle e1 e2)   = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ Ehandle e1' e2'
  eliminateLetSubPatterns e                 =
    return e