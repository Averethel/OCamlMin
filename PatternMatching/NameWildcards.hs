{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.NameWildcards (nameWildcards) where
  import Syntax

  import PatternMatching.Counters

  import Control.Monad.State

  nameWildcardsPattern :: MonadState Counter m => Pattern -> m Pattern
  nameWildcardsPattern Pwildcard     = do
    v <- freshWildcard
    return $ Pvar v
  nameWildcardsPattern (Ppair p1 p2) = do
    p1' <- nameWildcardsPattern p1
    p2' <- nameWildcardsPattern p2
    return $ Ppair p1' p2'
  nameWildcardsPattern (Pcons p1 p2) = do
    p1' <- nameWildcardsPattern p1
    p2' <- nameWildcardsPattern p2
    return $ Pcons p1' p2'
  nameWildcardsPattern p             =
    return p

  nameWildcardsFunClause :: MonadState Counter m => FunClause -> m FunClause
  nameWildcardsFunClause fc = do
    p' <- mapM nameWildcardsPattern $ arguments fc
    b' <- nameWildcards $ fbody fc
    return FC{ arguments = p', fbody = b' }

  nameWildcardsCaseClause :: MonadState Counter m => CaseClause -> m CaseClause
  nameWildcardsCaseClause cc = do
    b' <- nameWildcards $ cbody cc
    return cc{ cbody = b' }

  nameWildcards :: MonadState Counter m => Expr -> m Expr
  nameWildcards (Efun fcs)        = do
    fcs' <- mapM nameWildcardsFunClause fcs
    return $ Efun fcs'
  nameWildcards (Elet p e1 e2)    = do
    p'  <- nameWildcardsPattern p
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ Elet p' e1' e2'
  nameWildcards (Eletrec n fcs e) = do
    fcs' <- mapM nameWildcardsFunClause fcs
    e'   <- nameWildcards e
    return $ Eletrec n fcs' e'
  nameWildcards (Eapply e1 as)    = do
    e1' <- nameWildcards e1
    as' <- mapM nameWildcards as
    return $ Eapply e1' as'
  nameWildcards (Epair e1 e2)     = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ Epair e1' e2'
  nameWildcards (Econs e1 e2)     = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ Econs e1' e2'
  nameWildcards (Eif e1 e2 e3)    = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    e3' <- nameWildcards e3
    return $ Eif e1' e2' e3'
  nameWildcards (Eseq e1 e2)      = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ Eseq e1' e2'
  nameWildcards (Ecase e1 ccs)    = do
    e1'  <- nameWildcards e1
    ccs' <- mapM nameWildcardsCaseClause ccs
    return $ Ecase e1' ccs'
  nameWildcards (Ehandle e1 e2)   = do
    e1' <- nameWildcards e1
    e2' <- nameWildcards e2
    return $ Ehandle e1' e2'
  nameWildcards e                 =
    return e
