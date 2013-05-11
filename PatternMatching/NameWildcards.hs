{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.NameWildcards (nameWildcards) where
  import CompilerState
  import TypedSyntax

  import Control.Monad.State

  nameWildcardsPattern :: MonadState CompilerState m => TypedPattern -> m TypedPattern
  nameWildcardsPattern (TPwildcard t)   = do
    v <- freshWildcard t
    return $ TPvar v t
  nameWildcardsPattern (TPpair p1 p2 t) = do
    p1' <- nameWildcardsPattern p1
    p2' <- nameWildcardsPattern p2
    return $ TPpair p1' p2' t
  nameWildcardsPattern (TPcons p1 p2 t) = do
    p1' <- nameWildcardsPattern p1
    p2' <- nameWildcardsPattern p2
    return $ TPcons p1' p2' t
  nameWildcardsPattern p                =
    return p

  nameWildcardsFunClause :: MonadState CompilerState m => TypedFunClause ->
                            m TypedFunClause
  nameWildcardsFunClause fc = do
    p' <- mapM nameWildcardsPattern $ tfcArguments fc
    b' <- nameWildcards $ tfcBody fc
    return TFC { tfcArguments = p', tfcBody = b' }

  nameWildcardsCaseClause :: MonadState CompilerState m => TypedCaseClause ->
                             m TypedCaseClause
  nameWildcardsCaseClause cc = do
    b' <- nameWildcards $ tccBody cc
    return cc { tccBody = b' }

  nameWildcards :: MonadState CompilerState m => TypedExpr -> m TypedExpr
  nameWildcards (TEfun fcs t)             = do
    fcs' <- mapM nameWildcardsFunClause fcs
    return $ TEfun fcs' t
  nameWildcards (TElet p e1 e2 t)         = do
    p'  <- nameWildcardsPattern p
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ TElet p' e1' e2' t
  nameWildcards (TEletrec n t1 fcs e t2)  = do
    fcs' <- mapM nameWildcardsFunClause fcs
    e'   <- nameWildcards e
    return $ TEletrec n t1 fcs' e' t2
  nameWildcards (TEapply e1 as t)         = do
    e1' <- nameWildcards e1
    as' <- mapM nameWildcards as
    return $ TEapply e1' as' t
  nameWildcards (TEpair e1 e2 t)          = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ TEpair e1' e2' t
  nameWildcards (TEcons e1 e2 t)          = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ TEcons e1' e2' t
  nameWildcards (TEif e1 e2 e3 t)         = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    e3' <- nameWildcards e3
    return $ TEif e1' e2' e3' t
  nameWildcards (TEseq e1 e2 t)           = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ TEseq e1' e2' t
  nameWildcards (TEcase e1 ccs t)         = do
    e1'  <- nameWildcards e1
    ccs' <- mapM nameWildcardsCaseClause ccs
    return $ TEcase e1' ccs' t
  nameWildcards (TEhandle e1 e2 t)        = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ TEhandle e1' e2' t
  nameWildcards e                         =
    return e
