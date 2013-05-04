{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference (
  typeOfExpression,
  emptyEnv
) where
  import Counters
  import Syntax
  import TypedSyntax

  import TypeInference.Env
  import TypeInference.Expr
  import TypeInference.Constraints

  import Control.Monad.Error
  import Control.Monad.State

  typeOfExpression :: MonadState Counter m => Env -> Expr -> m (Either String TypedExpr)
  typeOfExpression env e = do
    errOrRes <- runErrorT $ typeOfExpr env emptyConstraints e
    return $ case errOrRes of
      Left s       -> Left s
      Right (a, _) -> Right a
