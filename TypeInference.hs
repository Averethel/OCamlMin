{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference (
  typeOfExpression,
  emptyEnv
) where
  import CompilerState
  import Syntax
  import TypedSyntax

  import TypeInference.Env
  import TypeInference.Expr
  import TypeInference.Constraints

  import Control.Monad.Error
  import Control.Monad.State

  typeOfExpression :: (MonadState CompilerState m, MonadError String m) => Env -> Expr -> m TypedExpr
  typeOfExpression env e = do
    errOrRes <- runErrorT $ typeOfExpr env emptyConstraints e
    case errOrRes of
      Left s       -> fail s
      Right (a, _) -> return a
