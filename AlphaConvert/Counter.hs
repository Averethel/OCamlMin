{-# LANGUAGE
  FlexibleContexts
  #-}

module AlphaConvert.Counter where
  import Control.Monad.State

  type Counter = Integer

  emptyState :: Counter
  emptyState = 0

  freshVar :: MonadState Counter m => String -> m String
  freshVar x = do
    s <- get
    put $ s + 1
    return $ x ++ '_' : show s
