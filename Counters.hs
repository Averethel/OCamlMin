{-# LANGUAGE
  FlexibleContexts
  #-}

module Counters where
  import Types
  import SPARC.Syntax

  import Control.Monad.State
  import Data.Map (Map, fromList, (!))

  data Counter = Cs {
    errorLabel    :: Integer,
    handledLabel  :: Integer,
    typeVar       :: Integer,
    variable      :: Integer
  }

  emptyState :: Counter
  emptyState = Cs {
    errorLabel    = 0,
    handledLabel  = 0,
    typeVar       = 0,
    variable      = 0
  }

  stateIncMap :: Map String (Counter -> Counter)
  stateIncMap = fromList [
    ("errorLabel",   \r -> r { errorLabel   = errorLabel   r + 1 }),
    ("handledLabel", \r -> r { handledLabel = handledLabel r + 1 }),
    ("typeVar",      \r -> r { typeVar      = typeVar      r + 1 }),
    ("variable",     \r -> r { variable     = variable     r + 1 }) ]

  incField :: Counter -> String -> Counter
  incField rec fieldName = (stateIncMap ! fieldName) rec


  getC :: MonadState Counter m => (Counter -> a) -> m a
  getC field = do
    s <- get
    return . field $ s

  incC :: MonadState Counter m => String -> m ()
  incC fieldName = do
    s <- get
    put $ s `incField` fieldName

  freshName :: MonadState Counter m => String -> m String
  freshName prefix = do
    n <- getC variable
    incC "variable"
    return $ prefix ++ '_' : show n

  freshTypeVar :: MonadState Counter m => m Type
  freshTypeVar = do
    n <- getC typeVar
    incC "typeVar"
    return $ Tvar $ 'a' : show n

  freshArg :: MonadState Counter m => Type -> m String
  freshArg t = freshName $ "_a_" ++ genId t

  genNames :: MonadState Counter m => [Type] -> m [String]
  genNames = mapM freshArg

  freshPMVar :: MonadState Counter m => Type -> m String
  freshPMVar t = freshName $ "_u_" ++ genId t

  freshWildcard :: MonadState Counter m => Type -> m String
  freshWildcard t = freshName $ "_w_" ++ genId t

  freshKVar :: MonadState Counter m => Type -> m String
  freshKVar t = freshName $ "_K_" ++ genId t

  freshLambda :: MonadState Counter m => Type -> m String
  freshLambda t = freshName $ "_Lam_" ++ genId t

  nextErrorLabel :: MonadState Counter m => m Label
  nextErrorLabel = do
    n <- getC errorLabel
    incC "errorLabel"
    return $ L $ "_error_" ++ show n

  nextHandledLabel :: MonadState Counter m => m Label
  nextHandledLabel = do
    n <- getC handledLabel
    incC "handledLabel"
    return $ L $ "_handled_" ++ show n
