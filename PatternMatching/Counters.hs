{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.Counters where
  import Control.Monad.State

  data Counter = C {
    args :: Integer,
    vars :: Integer,
    wild :: Integer
  }

  emptyState :: Counter
  emptyState = C 0 0 0

  freshArg :: MonadState Counter m => m String
  freshArg = do
    s <- get
    put s { args = args s + 1 }
    return $ '_' : 'a' : show (args s)

  genNames :: MonadState Counter m => Int -> m [String]
  genNames n = genNames' n [] where
    genNames' 0 acc = return $ reverse acc
    genNames' x acc = do
      a <- freshArg
      genNames' (x-1) $ a:acc

  freshVar :: MonadState Counter m => m String
  freshVar = do
    s <- get
    put s { vars = vars s + 1 }
    return $ '_' : 'u' : show (vars s)

  freshWildcard :: MonadState Counter m => m String
  freshWildcard = do
    s <- get
    put s { wild = wild s + 1 }
    return $ '_' : 'w' : show (wild s)
