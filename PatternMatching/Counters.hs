{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.Counters where
  import Control.Monad.State

  import Types

  data Counter = C {
    args :: Integer,
    vars :: Integer,
    wild :: Integer
  }

  emptyState :: Counter
  emptyState = C 0 0 0

  freshArg :: MonadState Counter m => Type -> m String
  freshArg t = do
    s <- get
    put s { args = args s + 1 }
    return $ '_' : 'a' : '_' : genId t ++ show (args s)

  genNames :: MonadState Counter m => [Type] -> m [String]
  genNames n = genNames' n [] where
    genNames' []     acc = return $ reverse acc
    genNames' (t:ts) acc = do
      a <- freshArg t
      genNames' ts $ a:acc

  freshVar :: MonadState Counter m => Type -> m String
  freshVar t = do
    s <- get
    put s { vars = vars s + 1 }
    return $ '_' : 'u' : '_' : genId t ++ show (vars s)

  freshWildcard :: MonadState Counter m => Type -> m String
  freshWildcard t = do
    s <- get
    put s { wild = wild s + 1 }
    return $ '_' : 'w' : '_' : genId t ++ show (wild s)
