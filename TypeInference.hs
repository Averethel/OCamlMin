module TypeInference where
  import Syntax
  import Types

  import TypeInference.Env
  import TypeInference.Expr
  import TypeInference.Constraints
  import TypeInference.Counter

  import Control.Monad.Error
  import Control.Monad.State

  typeOfExpression :: Env -> Expr -> Either String Type
  typeOfExpression env e =
    case fst $ runState (runErrorT $ typeOfExpr env emptyConstraints e) emptyState of
      Left s       -> Left s
      Right (a, _) -> Right a