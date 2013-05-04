{-# LANGUAGE
  FlexibleContexts
  #-}

module AlphaConvert.AlphaConvert (alphaConvert) where
  import AlphaConvert.Env
  import KNormal.KSyntax

  import Counters
  import Types

  import Control.Arrow
  import Control.Monad.State

  convertLet :: MonadState Counter m => Env ->
                ((String, Type) -> (String, Type) -> (String, Type) -> KExpr ->
                Type -> KExpr) -> (String, Type) -> (String, Type) ->
                (String, Type) -> KExpr -> Type -> m KExpr
  convertLet env letC (s1, t1) (s2, t2) (s3, t3) e t = do
    x1' <- freshName s1
    x2' <- freshName s2
    e'  <- alphaConvert (extend (extend env s1 x1') s2 x2') e
    return $ letC (x1', t1) (x2', t2) (env `find` s3, t3) e' t

  alphaConvert :: MonadState Counter m => Env -> KExpr -> m KExpr
  alphaConvert env (KEneg (s, t1) t2)                     =
    return $ KEneg (env `find` s, t1) t2
  alphaConvert env (KEload (s, t1) t2)                    =
    return $ KEload (env `find` s, t1) t2
  alphaConvert env (KEadd (s1, t1) (s2, t2) t3)           =
    return $ KEadd (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEsub (s1, t1) (s2, t2) t3)           =
    return $ KEsub (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEmult (s1, t1) (s2, t2) t3)          =
    return $ KEmult (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEdiv (s1, t1) (s2, t2) t3)           =
    return $ KEdiv (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEmod (s1, t1) (s2, t2) t3)           =
    return $ KEmod (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEstore (s1, t1) (s2, t2) t3)         =
    return $ KEstore (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEvar s t)                            =
    return $ KEvar (env `find` s) t
  alphaConvert env (KEifEq (s1, t1) (s2, t2) e1 e2 t3)    = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEifEq (env `find` s1, t1) (env `find` s2, t2) e1' e2' t3
  alphaConvert env (KEifLE (s1, t1) (s2, t2) e1 e2 t3)    = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEifLE (env `find` s1, t1) (env `find` s2, t2) e1' e2' t3
  alphaConvert env (KElet (s, t1) e1 e2 t2)               = do
    x'  <- freshName s
    e1' <- alphaConvert env e1
    e2' <- alphaConvert (extend env s x') e2
    return $ KElet (x', t1) e1' e2' t2
  alphaConvert env (KEletRec fd e t)                      = do
    x'  <- freshName . fst . name $ fd
    mps <- mapM (\(x, _) -> do { i <- freshName x; return (x, i) }) $ args fd
    let env'  = extend env (fst . name $ fd) x'
    let env'' = addList env' mps
    b'  <- alphaConvert env'' $ body fd
    e'  <- alphaConvert env' e
    return $ KEletRec FD{ name = (env' `find` (fst . name $ fd),
                                    snd . name $ fd),
                          args = map (first $ find env'') $ args fd,
                          body = b' } e' t
  alphaConvert env (KEapply (s, t1) ss t2)                =
    return $ KEapply (env `find` s, t1) (map (first $ find env) ss) t2
  alphaConvert env (KEpair (s1, t1) (s2, t2) t3)          =
    return $ KEpair (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEcons (s1, t1) (s2, t2) t3)          =
    return $ KEcons (env `find` s1, t1) (env `find` s2, t2) t3
  alphaConvert env (KEletPair s1 s2 s3 e t)               =
    convertLet env KEletPair s1 s2 s3 e t
  alphaConvert env (KEletList s1 s2 s3 e t)               =
    convertLet env KEletList s1 s2 s3 e t
  alphaConvert env (KEhandle e1 e2 t)                     = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEhandle e1' e2' t
  alphaConvert env (KEseq e1 e2 t)                        = do
    e1' <- alphaConvert env e1
    e2' <- alphaConvert env e2
    return $ KEseq e1' e2' t
  alphaConvert env (KEextFunApp es ss t)                  =
    return $ KEextFunApp es (map (first $ find env) ss) t
  alphaConvert _   e                                      =
    return e
