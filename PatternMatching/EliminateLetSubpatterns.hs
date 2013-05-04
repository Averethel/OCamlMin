{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.EliminateLetSubpatterns (eliminateLetSubPatterns) where
  import Counters
  import TypedSyntax.Expr
  import TypedSyntax.Pattern

  import Types

  import Control.Monad.State

  getSubPatterns :: MonadState Counter m =>
                    TypedPattern -> TypedExpr ->
                    m [(TypedPattern, TypedExpr)]
  getSubPatterns (TPpair p1 p2 t) e = getSubPatterns' TPpair p1 p2 t e
  getSubPatterns (TPcons p1 p2 t) e = getSubPatterns' TPcons p1 p2 t e
  getSubPatterns p                e = return [(p, e)]

  getSubPatterns' :: MonadState Counter m =>
                     (TypedPattern -> TypedPattern -> Type -> TypedPattern) ->
                     TypedPattern -> TypedPattern -> Type -> TypedExpr ->
                     m [(TypedPattern, TypedExpr)]
  getSubPatterns' cons p1 p2 t e
    | isAtomicTypedPattern p1 &&
      isAtomicTypedPattern p2           = return [(cons p1 p2 t, e)]
    | not (isAtomicTypedPattern p1) &&
      isAtomicTypedPattern p2           = do
        let t' = typeOfTypedPattern p1
        v  <- freshPMVar t'
        ps <- getSubPatterns p1 (TEvar v t')
        return $ (cons (TPvar v t') p2 t, e) : ps
    | isAtomicTypedPattern p1 &&
      not (isAtomicTypedPattern p2)    = do
        let t' = typeOfTypedPattern p2
        v  <- freshPMVar t'
        ps <- getSubPatterns p2 (TEvar v t')
        return $ (cons p1 (TPvar v t') t, e) : ps
    | otherwise                        = do
      let t1' = typeOfTypedPattern p1
      let t2' = typeOfTypedPattern p2
      v1  <- freshPMVar t1'
      v2  <- freshPMVar t2'
      ps1 <- getSubPatterns p1 (TEvar v1 t1')
      ps2 <- getSubPatterns p2 (TEvar v2 t2')
      return $ (cons (TPvar v1 t1') (TPvar v2 t2') t, e) : (ps1 ++ ps2)

  eliminateLetSubPatternsFunClause :: MonadState Counter m =>
                                      TypedFunClause -> m TypedFunClause
  eliminateLetSubPatternsFunClause fc = do
    b' <- eliminateLetSubPatterns $ tfcBody fc
    return fc{ tfcBody = b' }

  eliminateLetSubPatternsCaseClause :: MonadState Counter m =>
                                       TypedCaseClause -> m TypedCaseClause
  eliminateLetSubPatternsCaseClause cc = do
    b' <- eliminateLetSubPatterns $ tccBody cc
    return cc{ tccBody = b' }

  eliminateLetSubPatterns :: MonadState Counter m => TypedExpr -> m TypedExpr
  eliminateLetSubPatterns (TEfun fcs t)             = do
    fcs' <- mapM eliminateLetSubPatternsFunClause fcs
    return $ TEfun fcs' t
  eliminateLetSubPatterns (TElet p e1 e2 t)         = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    if   not (hasSubPatterns p)
    then return $ TElet p e1' e2' t
    else do
      ps <- getSubPatterns p e1'
      return $
        foldr (\(px, ex) ey -> TElet px ex ey $ typeOfTypedExpr ey) e2' ps
  eliminateLetSubPatterns (TEletrec n t1 fcs e t2)  = do
    fcs' <- mapM eliminateLetSubPatternsFunClause fcs
    e'   <- eliminateLetSubPatterns e
    return $ TEletrec n t1 fcs' e' t2
  eliminateLetSubPatterns (TEapply e1 as t)         = do
    e1' <- eliminateLetSubPatterns e1
    as' <- mapM eliminateLetSubPatterns as
    return $ TEapply e1' as' t
  eliminateLetSubPatterns (TEpair e1 e2 t)          = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ TEpair e1' e2' t
  eliminateLetSubPatterns (TEcons e1 e2 t)          = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ TEcons e1' e2' t
  eliminateLetSubPatterns (TEif e1 e2 e3 t)         = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    e3' <- eliminateLetSubPatterns e3
    return $ TEif e1' e2' e3' t
  eliminateLetSubPatterns (TEseq e1 e2 t)           = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ TEseq e1' e2' t
  eliminateLetSubPatterns (TEcase e1 ccs t)         = do
    e1'  <- eliminateLetSubPatterns e1
    ccs' <- mapM eliminateLetSubPatternsCaseClause ccs
    return $ TEcase e1' ccs' t
  eliminateLetSubPatterns (TEhandle e1 e2 t)        = do
    e1' <- eliminateLetSubPatterns e1
    e2' <- eliminateLetSubPatterns e2
    return $ TEhandle e1' e2' t
  eliminateLetSubPatterns e                         =
    return e