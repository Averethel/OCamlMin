{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.ToHandles where
  import CompilerState
  import TypedSyntax.Expr
  import TypedSyntax.Pattern

  import Control.Monad.State

  functionsToHandlesFunClause :: MonadState CompilerState m => TypedFunClause ->
                                 m TypedExpr
  functionsToHandlesFunClause fc = do
    b' <- functionsToHandles $ tfcBody fc
    return $ TEfun [fc { tfcBody  = b' }] $ typeOfTypedFunClause fc

  functionsToHandlesFunClauses :: MonadState CompilerState m =>
                                  [TypedFunClause] -> m [TypedFunClause]
  functionsToHandlesFunClauses fcs = do
    fcs'   <- mapM functionsToHandlesFunClause fcs
    let types = map typeOfTypedPattern $ tfcArguments . head $ fcs
    let rtype = typeOfTypedExpr . tfcBody . head $ fcs
    ns'    <- genNames types
    let fcs'' = map (\c -> TEapply c (zipWith TEvar ns' types) rtype) fcs'
    return [TFC { tfcArguments = zipWith TPvar ns' types,
                  tfcBody      = foldr (\a b -> TEhandle a b rtype)
                                        (TEmatchFailure rtype) fcs'' }]


  functionsToHandles :: MonadState CompilerState m => TypedExpr -> m TypedExpr
  functionsToHandles (TEfun fcs t)            = do
    fcs' <- functionsToHandlesFunClauses fcs
    return $ TEfun fcs' t
  functionsToHandles (TElet p e1 e2 t)        = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ TElet p e1' e2' t
  functionsToHandles (TEletrec n t1 fcs e t2) = do
    fcs' <- functionsToHandlesFunClauses fcs
    e'   <- functionsToHandles e
    return $ TEletrec n t1 fcs' e' t2
  functionsToHandles (TEapply e1 as t)        = do
    e1' <- functionsToHandles e1
    as' <- mapM functionsToHandles as
    return $ TEapply e1' as' t
  functionsToHandles (TEpair e1 e2 t)         = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ TEpair e1' e2' t
  functionsToHandles (TEcons e1 e2 t)         = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ TEcons e1' e2' t
  functionsToHandles (TEif e1 e2 e3 t)        = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    e3' <- functionsToHandles e3
    return $ TEif e1' e2' e3' t
  functionsToHandles (TEseq e1 e2 t)          = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ TEseq e1' e2' t
  functionsToHandles (TEhandle e1 e2 t)       = do
    e1' <- functionsToHandles e1
    e2' <- functionsToHandles e2
    return $ TEhandle e1' e2' t
  functionsToHandles e                        = return e
