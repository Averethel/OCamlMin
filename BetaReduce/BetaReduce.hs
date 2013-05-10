module BetaReduce.BetaReduce where
  import AlphaConvert.Env
  import KNormal.KSyntax

  import Types

  import Control.Arrow

  reduceIf :: Env -> ((String, Type) -> (String, Type) -> KExpr -> KExpr ->
              Type -> KExpr) -> (String, Type) -> (String, Type) -> KExpr ->
              KExpr -> Type -> IO KExpr
  reduceIf env cmp (s1, t1) (s2, t2) e1 e2 t3 = do
    e1' <- betaReduce env e1
    e2' <- betaReduce env e2
    return $ cmp (env `find` s1, t1) (env `find` s2, t2) e1' e2' t3

  reduceLet :: Env -> ((String, Type) -> (String, Type) -> (String, Type) ->
               KExpr -> Type -> KExpr) -> (String, Type) -> (String, Type) ->
              (String, Type) -> KExpr -> Type -> IO KExpr
  reduceLet env clet s1 s2 (s3, t3) e t = do
    e' <- betaReduce env e
    return $ clet s1 s2 (env `find` s3, t3) e' t

  betaReduce :: Env -> KExpr -> IO KExpr
  betaReduce env (KEneg (s, t1) t2)                     =
    return $ KEneg (env `find` s, t1) t2
  betaReduce env (KEload (s, t1) t2)                    =
    return $ KEload (env `find` s, t1) t2
  betaReduce env (KEadd (s1, t1) (s2, t2) t3)           =
    return $ KEadd (env `find` s1, t1) (env `find` s2, t2) t3
  betaReduce env (KEsub (s1, t1) (s2, t2) t3)           =
    return $ KEsub (env `find` s1, t1) (env `find` s2, t2) t3
  betaReduce env (KEmult (s1, t1) (s2, t2) t3)          =
    return $ KEmult (env `find` s1, t1) (env `find` s2, t2) t3
  betaReduce env (KEdiv (s1, t1) (s2, t2) t3)           =
    return $ KEdiv (env `find` s1, t1) (env `find` s2, t2) t3
  betaReduce env (KEstore (s1, t1) (s2, t2) t3)         =
    return $ KEstore (env `find` s1, t1) (env `find` s2, t2) t3
  betaReduce env (KEvar s t)                            =
    return $ KEvar (env `find` s) t
  betaReduce env (KEifEq (s1, t1) (s2, t2) t3 e1 e2)    =
    reduceIf env KEifEq (s1, t1) (s2, t2) t3 e1 e2
  betaReduce env (KEifLE (s1, t1) (s2, t2) t3 e1 e2)    =
    reduceIf env KEifLE (s1, t1) (s2, t2) t3 e1 e2
  betaReduce env (KElet (s, t1) e1 e2 t2)               = do
    e1' <- betaReduce env e1
    case e1' of
      KEvar x _ -> do
        putStrLn $ "Beta reducing " ++ show s ++ " = " ++ show x ++ "."
        betaReduce (extend env s x) e2
      _       -> do
        e2' <- betaReduce env e2
        return $ KElet (s, t1) e1' e2' t2
  betaReduce env (KEletRec fd e t)                      = do
    b' <- betaReduce env $ body fd
    e' <- betaReduce env e
    return $ KEletRec fd { body = b' } e' t
  betaReduce env (KEapply (s, t1) ss t2)                =
    return $ KEapply (env `find` s, t1) (map (first $ find env) ss) t2
  betaReduce env (KEpair (s1, t1) (s2, t2) t3)          =
    return $ KEpair (env `find` s1, t1) (env `find` s2, t2) t3
  betaReduce env (KEcons (s1, t1) (s2, t2) t3)          =
    return $ KEcons (env `find` s1, t1) (env `find` s2, t2) t3
  betaReduce env (KEletPair (s1, t1) (s2, t2) t3 s3 e)  =
    reduceLet env KEletPair (s1, t1) (s2, t2) t3 s3 e
  betaReduce env (KEletList (s1, t1) (s2, t2) t3 s3 e)  =
    reduceLet env KEletList (s1, t1) (s2, t2) t3 s3 e
  betaReduce env (KEhandle e1 e2 t)                     = do
    e1' <- betaReduce env e1
    e2' <- betaReduce env e2
    return $ KEhandle e1' e2' t
  betaReduce env (KEseq e1 e2 t)                        = do
    e1' <- betaReduce env e1
    e2' <- betaReduce env e2
    return $ KEseq e1' e2' t
  betaReduce env (KEextFunApp s ss t)                   =
    return $ KEextFunApp s (map (first $ find env) ss) t
  betaReduce _   e                                      =
    return e
