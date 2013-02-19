module BetaReduce.BetaReduce where
  import AlphaConvert.Env
  import KNormal.KSyntax

  reduceIf :: Env -> (String -> String -> KExpr -> KExpr -> KExpr) ->
              String -> String -> KExpr -> KExpr -> IO KExpr
  reduceIf env cmp s1 s2 e1 e2 = do
    e1' <- betaReduce env e1
    e2' <- betaReduce env e2
    return $ cmp (env `find` s1) (env `find` s2) e1' e2'

  reduceLet :: Env -> (String -> String -> String -> KExpr -> KExpr) ->
               String -> String -> String -> KExpr -> IO KExpr
  reduceLet env clet s1 s2 s3 e = do
    e' <- betaReduce env e
    return $ clet s1 s2 (env `find` s3) e'

  betaReduce :: Env -> KExpr -> IO KExpr
  betaReduce env (KEneg s)              =
    return $ KEneg $ env `find` s
  betaReduce env (KEload s)             =
    return $ KEload $ env `find` s
  betaReduce env (KEadd s1 s2)          =
    return $ KEadd (env `find` s1) $ env `find` s2
  betaReduce env (KEsub s1 s2)          =
    return $ KEsub (env `find` s1) $ env `find` s2
  betaReduce env (KEmult s1 s2)         =
    return $ KEmult (env `find` s1) $ env `find` s2
  betaReduce env (KEdiv s1 s2)          =
    return $ KEdiv (env `find` s1) $ env `find` s2
  betaReduce env (KEmod s1 s2)          =
    return $ KEmod (env `find` s1) $ env `find` s2
  betaReduce env (KEstore s1 s2)        =
    return $ KEstore (env `find` s1) $ env `find` s2
  betaReduce env (KEvar s)              =
    return $ KEvar $ env `find` s
  betaReduce env (KEifEq s1 s2 e1 e2)   =
    reduceIf env KEifEq s1 s2 e1 e2
  betaReduce env (KEifLE s1 s2 e1 e2)   =
    reduceIf env KEifLE s1 s2 e1 e2
  betaReduce env (KElet s e1 e2)        = do
    e1' <- betaReduce env e1
    case e1' of
      KEvar x -> do
        putStrLn $ "Beta reducing " ++ show s ++ " = " ++ show x ++ "."
        betaReduce (extend env s x) e2
      _       -> do
        e2' <- betaReduce env e2
        return $ KElet s e1' e2'
  betaReduce env (KEletRec fd e)        = do
    b' <- betaReduce env $ body fd
    e' <- betaReduce env e
    return $ KEletRec fd { body = b' } e'
  betaReduce env (KEapply s ss)         =
    return $ KEapply (env `find` s) $ map (find env) ss
  betaReduce env (KEpair s1 s2)         =
    return $ KEpair (env `find` s1) $ env `find` s2
  betaReduce env (KEcons s1 s2)         =
    return $ KEcons (env `find` s1) $ env `find` s2
  betaReduce env (KEletPair s1 s2 s3 e) =
    reduceLet env KEletPair s1 s2 s3 e
  betaReduce env (KEletList s1 s2 s3 e) =
    reduceLet env KEletList s1 s2 s3 e
  betaReduce env (KEhandle e1 e2)       = do
    e1' <- betaReduce env e1
    e2' <- betaReduce env e2
    return $ KEhandle e1' e2'
  betaReduce env (KEseq e1 e2)          = do
    e1' <- betaReduce env e1
    e2' <- betaReduce env e2
    return $ KEseq e1' e2'
  betaReduce env (KEextFunApp s ss)     =
    return $ KEextFunApp s $ map (find env) ss
  betaReduce _   e                      =
    return e
