{-# LANGUAGE
  FlexibleContexts
  #-}

module KNormal.Counter where
  import Control.Monad.State

  data Counter = C {
    variable :: Integer,
    lambda   :: Integer
  }

  emptyState :: Counter
  emptyState = C 0 0

  freshVar :: MonadState Counter m => m String
  freshVar = do
    s <- get
    put s { variable = variable s + 1 }
    return $ '_':'K': show (variable s)

  freshLambda :: MonadState Counter m => m String
  freshLambda = do
    s <- get
    put s { lambda = lambda s + 1 }
    return $ '_':'L': 'a' : 'm' : show (lambda s)
