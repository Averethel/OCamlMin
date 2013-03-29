{-# LANGUAGE
  FlexibleContexts
  #-}

module KNormal.Counter where
  import Control.Monad.State

  import Types

  data Counter = C {
    variable :: Integer,
    lambda   :: Integer
  }

  emptyState :: Counter
  emptyState = C 0 0

  freshVar :: MonadState Counter m => Type -> m String
  freshVar t = do
    s <- get
    put s { variable = variable s + 1 }
    return $ '_':'K':'_':genId t ++ show (variable s)

  freshLambda :: MonadState Counter m => Type -> m String
  freshLambda t = do
    s <- get
    put s { lambda = lambda s + 1 }
    return $ '_':'L':'a':'m':'_':genId t ++ show (lambda s)
