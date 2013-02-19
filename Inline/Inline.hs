module Inline.Inline where
  import Inline.Env
  import Inline.Size

  import KNormal.KSyntax
  import AlphaConvert

  inline :: Env -> Integer -> KExpr -> IO KExpr
  inline env trs (KEifEq s1 s2 e1 e2)   = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEifEq s1 s2 e1' e2'
  inline env trs (KEifLE s1 s2 e1 e2)   = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEifLE s1 s2 e1' e2'
  inline env trs (KElet s e1 e2)        = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KElet s e1' e2'
  inline env trs (KEletRec fd e)        = do
    b' <- inline env trs $ body fd
    e' <- inline env' trs e
    return $ KEletRec fd { body = b' } e'
    where
      env'
        | size (body fd) > trs = env
        | otherwise            = env `extend` fd
  inline env trs (KEapply s ss)
    | s `member` env                    = do
      putStrLn $ "Inlining " ++ show s ++ "."
      let (fas, b) = env `find` s
      inline env trs $ alphaConvertWithEnv (zip fas ss) b
  inline env trs (KEletPair s1 s2 s3 e) = do
    e' <- inline env trs e
    return $ KEletPair s1 s2 s3 e'
  inline env trs (KEletList s1 s2 s3 e) = do
    e' <- inline env trs e
    return $ KEletList s1 s2 s3 e'
  inline env trs (KEhandle e1 e2)       = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEhandle e1' e2'
  inline env trs (KEseq e1 e2)          = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEseq e1' e2'
  inline _   _   e                      =
    return e
