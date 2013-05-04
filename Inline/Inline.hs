{-# LANGUAGE
  FlexibleContexts
  #-}

module Inline.Inline where
  import Inline.Env
  import Inline.Size

  import KNormal.KSyntax
  import AlphaConvert
  import Counters

  import Control.Monad.State

  inline :: (MonadIO m, MonadState Counter m) =>
            Env -> Integer -> KExpr -> m KExpr
  inline env trs (KEifEq s1 s2 e1 e2 t)   = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEifEq s1 s2 e1' e2' t
  inline env trs (KEifLE s1 s2 e1 e2 t)   = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEifLE s1 s2 e1' e2' t
  inline env trs (KElet s e1 e2 t)        = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KElet s e1' e2' t
  inline env trs (KEletRec fd e t)        = do
    b' <- inline env trs $ body fd
    e' <- inline env' trs e
    return $ KEletRec fd { body = b' } e' t
    where
      env'
        | size (body fd) > trs = env
        | otherwise            = env `extend` fd
  inline env trs (KEapply (s, _) ss _)
    | s `member` env                      = do
      liftIO $ putStrLn $ "Inlining " ++ show s ++ "."
      let (fas, b) = env `find` s
      e' <- alphaConvertWithEnv (zip fas $ map fst ss) b
      inline env trs e'
  inline env trs (KEletPair s1 s2 s3 e t) = do
    e' <- inline env trs e
    return $ KEletPair s1 s2 s3 e' t
  inline env trs (KEletList s1 s2 s3 e t) = do
    e' <- inline env trs e
    return $ KEletList s1 s2 s3 e' t
  inline env trs (KEhandle e1 e2 t)       = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEhandle e1' e2' t
  inline env trs (KEseq e1 e2 t)          = do
    e1' <- inline env trs e1
    e2' <- inline env trs e2
    return $ KEseq e1' e2' t
  inline _   _   e                        =
    return e
