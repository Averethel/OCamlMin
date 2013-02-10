{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Env (Env, emptyEnv, get, extend) where
  import Types
  import Utils.Errors

  import Control.Monad.Error

  type Env = [(String, Type)]

  emptyEnv :: Env
  emptyEnv = []

  get :: MonadError String m => String -> Env -> m Type
  get v env =
    case v `lookup` env of
      Nothing -> throwError $ unboundVariable v
      Just tp -> return tp

  extend :: Env -> (String, Type) -> Env
  extend = flip (:)
