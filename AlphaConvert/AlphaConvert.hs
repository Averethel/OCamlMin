{-# LANGUAGE
  FlexibleContexts
  #-}

module AlphaConvert.AlphaConvert (alphaConvert) where
  import AlphaConvert.Env
  import AlphaConvert.Counter

  import KNormal.KSyntax

  import Control.Monad.State

  convertLet :: MonadState Counter m =>
                Env -> (String -> String -> String -> KExpr -> KExpr) ->
                String -> String -> String -> KExpr -> m KExpr
  convertLet env letC s1 s2 s3 e = do
    x1' <- freshVar s1
    x2' <- freshVar s2
    e'  <- alphaConvert (extend (extend env s1 x1') s2 x2') e
    return $ letC x1' x2' (env `find` s3) e'

  alphaConvert :: MonadState Counter m => Env -> KExpr -> m KExpr
  alphaConvert env (KEneg s)              =
    return $ KEneg $ env `find` s
  alphaConvert env (KEload s)             =
    return $ KEload $ env `find` s
  alphaConvert env (KEadd s1 s2)          =
    return $ KEadd (env `find` s1) $ env `find` s2
  alphaConvert env (KEsub s1 s2)          =
    return $ KEsub (env `find` s1) $ env `find` s2
  alphaConvert env (KEmult s1 s2)         =
    return $ KEmult (env `find` s1) $ env `find` s2
  alphaConvert env (KEdiv s1 s2)          =
    return $ KEdiv (env `find` s1) $ env `find` s2
  alphaConvert env (KEmod s1 s2)          =
    return $ KEmod (env `find` s1) $ env `find` s2
  alphaConvert env (KEstore s1 s2)        =
    return $ KEstore (env `find` s1) $ env `find` s2
  alphaConvert env (KEvar s)              =
    return $ KEvar $ env `find` s
  alphaConvert env (KEifEq s1 s2 e1 e2)   = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEifEq (env `find` s1) (env `find` s2) e1' e2'
  alphaConvert env (KEifLE s1 s2 e1 e2)   = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEifLE (env `find` s1) (env `find` s2) e1' e2'
  alphaConvert env (KElet s e1 e2)        = do
    x'  <- freshVar s
    e1' <- alphaConvert env e1
    e2' <- alphaConvert (extend env s x') e2
    return $ KElet x' e1' e2'
  alphaConvert env (KEletRec fd e)        = do
    x'  <- freshVar $ name fd
    mps <- mapM (\x -> do { i <- freshVar x; return (x, i) }) $ args fd
    let env'  = extend env (name fd) x'
    let env'' = addList env' mps
    b'  <- alphaConvert env'' $ body fd
    e'  <- alphaConvert env' e
    return $ KEletRec FD{ name = env' `find` name fd,
                          args = map (find env'') $ args fd,
                          body = b' } e'
  alphaConvert env (KEapply s ss)         =
    return $ KEapply (env `find` s) $ map (find env) ss
  alphaConvert env (KEpair s1 s2)         =
    return $ KEpair (env `find` s1) $ env `find` s2
  alphaConvert env (KEcons s1 s2)         =
    return $ KEcons (env `find` s1) $ env `find` s2
  alphaConvert env (KEletPair s1 s2 s3 e) =
    convertLet env KEletPair s1 s2 s3 e
  alphaConvert env (KEletList s1 s2 s3 e) =
    convertLet env KEletList s1 s2 s3 e
  alphaConvert env (KEhandle e1 e2)       = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEhandle e1' e2'
  alphaConvert env (KEseq e1 e2)          = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEseq e1' e2'
  alphaConvert env (KEextFunApp es ss)    =
    return $ KEextFunApp es $ map (find env) ss
  alphaConvert _   e                      =
    return e
