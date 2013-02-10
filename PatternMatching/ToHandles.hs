{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.ToHandles where
  import Syntax.Expr
  import Syntax.Pattern

  import PatternMatching.Counters

  import Control.Monad.State

  functionsToHandlesFunClause :: MonadState Counter m => FunClause -> m Expr
  functionsToHandlesFunClause fc = do
    b' <- functionsToHandles $ fbody fc
    return $ Efun [fc { fbody  = b' }]

  functionsToHandlesFunClauses :: MonadState Counter m =>
                                  [FunClause] -> m [FunClause]
  functionsToHandlesFunClauses fcs = do
    fcs'   <- mapM functionsToHandlesFunClause fcs
    ns'    <- genNames . length . arguments . head $ fcs
    let fcs'' = map (`Eapply` map Evar ns') fcs'
    return [FC {arguments = map Pvar ns',
                fbody     = foldr Ehandle EmatchFailure fcs'' }]


  functionsToHandles :: MonadState Counter m => Expr -> m Expr
  functionsToHandles (Efun fcs)         = do
    fcs' <- functionsToHandlesFunClauses fcs
    return $ Efun fcs'
  functionsToHandles (Elet p e1 e2)     = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ Elet p e1' e2'
  functionsToHandles (Eletrec n fcs e)  = do
    fcs' <- functionsToHandlesFunClauses fcs
    e'   <- functionsToHandles e
    return $ Eletrec n fcs' e'
  functionsToHandles (Eapply e1 as)     = do
    e1' <- functionsToHandles e1
    as' <- mapM functionsToHandles as
    return $ Eapply e1' as'
  functionsToHandles (Epair e1 e2)      = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ Epair e1' e2'
  functionsToHandles (Econs e1 e2)      = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ Econs e1' e2'
  functionsToHandles (Eif e1 e2 e3)     = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    e3' <- functionsToHandles e3
    return $ Eif e1' e2' e3'
  functionsToHandles (Eseq e1 e2)       = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ Eseq e1' e2'
  functionsToHandles (Ehandle e1 e2)    = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ Ehandle e1' e2'
  functionsToHandles e                  = return e
