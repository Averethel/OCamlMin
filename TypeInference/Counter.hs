{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Counter where
  import Types

  import Control.Monad.State

  type Counter = Integer

  emptyState :: Counter
  emptyState = 0

  freshVar :: MonadState Counter m => m Type
  freshVar = do
    n <- get
    put $ n + 1
    return $ Tvar $ 'a' : show n
