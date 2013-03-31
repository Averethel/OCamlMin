module EliminateDefinitions (eliminateDefinitions) where
  import KNormal.KSyntax

  import Data.Set

  hasSideEffect  :: KExpr -> Bool
  hasSideEffect (KEstore{})           = True
  hasSideEffect (KEerror _ _)         = True
  hasSideEffect (KEifEq _ _ e1 e2 _)  = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEifLE _ _ e1 e2 _)  = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KElet _ e1 e2 _)     = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEletRec _ e _)      = hasSideEffect e
  hasSideEffect (KEapply{})           = True
  hasSideEffect (KEletPair _ _ _ e _) = hasSideEffect e
  hasSideEffect (KEletList _ _ _ e _) = hasSideEffect e
  hasSideEffect (KEhandle e1 e2 _)    = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEseq e1 e2 _)       = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEextFunApp{})       = True
  hasSideEffect _                     = False

  eliminateDefinitions :: KExpr -> IO KExpr
  eliminateDefinitions (KEifEq s1 s2 e1 e2 t)     = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEifEq s1 s2 e1' e2' t
  eliminateDefinitions (KEifLE s1 s2 e1 e2 t)     = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEifLE s1 s2 e1' e2' t
  eliminateDefinitions (KElet (s, t1) e1 e2 t2)
    | not (hasSideEffect e1) &&
      not (s `member` freeVars e2)                = do
        putStrLn $ "Eliminating variable: " ++ show s
        eliminateDefinitions e2
    | otherwise                                   = do
      e1' <- eliminateDefinitions e1
      e2' <- eliminateDefinitions e2
      return $ KElet (s, t1) e1' e2' t2
  eliminateDefinitions (KEletRec fd e t)
    | not ((fst . name $ fd) `member` freeVars e) = do
      putStrLn $ "Eliminating function: " ++ show (name fd)
      eliminateDefinitions e
    | otherwise                                   = do
      b' <- eliminateDefinitions $ body fd
      e' <- eliminateDefinitions e
      return $ KEletRec fd { body = b' } e' t
  eliminateDefinitions (KEletPair s1 s2 s3 e t)
    | not (fst s1 `member` freeVars e) &&
      not (fst s2 `member` freeVars e)            = do
        putStrLn $ "Eliminating variables: " ++ show s1 ++ ", " ++ show s2
        eliminateDefinitions e
    | otherwise                                   = do
      e' <- eliminateDefinitions e
      return $ KEletPair s1 s2 s3 e' t
  eliminateDefinitions (KEletList s1 s2 s3 e t)
    | not (fst s1 `member` freeVars e) &&
      not (fst s2 `member` freeVars e)            = do
        putStrLn $ "Eliminating variables: " ++ show s1 ++ ", " ++ show s2
        eliminateDefinitions e
    | otherwise                                   = do
      e' <- eliminateDefinitions e
      return $ KEletList s1 s2 s3 e' t
  eliminateDefinitions (KEhandle e1 e2 t)         = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEhandle e1' e2' t
  eliminateDefinitions (KEseq e1 e2 t)            = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEseq e1' e2' t
  eliminateDefinitions e                          =
    return e
