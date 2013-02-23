module EliminateDefinitions (eliminateDefinitions) where
  import KNormal.KSyntax

  import Data.Set

  hasSideEffect  :: KExpr -> Bool
  hasSideEffect (KEstore _ _)       = True
  hasSideEffect (KEerror _)         = True
  hasSideEffect (KEifEq _ _ e1 e2)  = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEifLE _ _ e1 e2)  = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KElet _ e1 e2)     = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEletRec _ e)      = hasSideEffect e
  hasSideEffect (KEapply _ _)       = True
  hasSideEffect (KEletPair _ _ _ e) = hasSideEffect e
  hasSideEffect (KEletList _ _ _ e) = hasSideEffect e
  hasSideEffect (KEhandle e1 e2)    = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEseq e1 e2)       = hasSideEffect e1 || hasSideEffect e2
  hasSideEffect (KEextFunApp _ _)   = True
  hasSideEffect _                   = False

  eliminateDefinitions :: KExpr -> IO KExpr
  eliminateDefinitions (KEifEq s1 s2 e1 e2)   = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEifEq s1 s2 e1' e2'
  eliminateDefinitions (KEifLE s1 s2 e1 e2)   = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEifLE s1 s2 e1' e2'
  eliminateDefinitions (KElet s e1 e2)
    | not (hasSideEffect e1) &&
      not (s `member` freeVars e2)            = do
        putStrLn $ "Eliminating variable: " ++ show s
        eliminateDefinitions e2
    | otherwise                               = do
      e1' <- eliminateDefinitions e1
      e2' <- eliminateDefinitions e2
      return $ KElet s e1' e2'
  eliminateDefinitions (KEletRec fd e)
    | not (name fd `member` freeVars e)       = do
      putStrLn $ "Eliminating function: " ++ show (name fd)
      eliminateDefinitions e
    | otherwise                               = do
      b' <- eliminateDefinitions $ body fd
      e' <- eliminateDefinitions e
      return $ KEletRec fd { body = b' } e'
  eliminateDefinitions (KEletPair s1 s2 s3 e)
    | not (s1 `member` freeVars e) &&
      not (s2 `member` freeVars e)            = do
        putStrLn $ "Eliminating variables: " ++ show s1 ++ ", " ++ show s2
        eliminateDefinitions e
    | otherwise                               = do
      e' <- eliminateDefinitions e
      return $ KEletPair s1 s2 s3 e'
  eliminateDefinitions (KEletList s1 s2 s3 e)
    | not (s1 `member` freeVars e) &&
      not (s2 `member` freeVars e)            = do
        putStrLn $ "Eliminating variables: " ++ show s1 ++ ", " ++ show s2
        eliminateDefinitions e
    | otherwise                               = do
      e' <- eliminateDefinitions e
      return $ KEletList s1 s2 s3 e'
  eliminateDefinitions (KEhandle e1 e2)       = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEhandle e1' e2'
  eliminateDefinitions (KEseq e1 e2)          = do
    e1' <- eliminateDefinitions e1
    e2' <- eliminateDefinitions e2
    return $ KEseq e1' e2'
  eliminateDefinitions e                      =
    return e
