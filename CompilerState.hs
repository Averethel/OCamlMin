{-# LANGUAGE
  FlexibleContexts
  #-}

module CompilerState where
  import Types
  import X86.Syntax

  import Control.Monad.State
  import Data.Map (Map, fromList, (!))
  import Data.Set (Set, empty)

  type StackSet = Set String
  type StackMap = [String]

  emptyStackSet :: StackSet
  emptyStackSet = empty

  emptyStackMap :: StackMap
  emptyStackMap = []

  data CompilerState = CS {
    errorLabel    :: Integer,
    handledLabel  :: Integer,
    typeVar       :: Integer,
    variable      :: Integer,
    stackMap      :: StackMap,
    stackSet      :: StackSet
  }

  emptyState :: CompilerState
  emptyState = CS {
    errorLabel    = 0,
    handledLabel  = 0,
    typeVar       = 0,
    variable      = 0,
    stackMap      = emptyStackMap,
    stackSet      = emptyStackSet
  }

  stateIncMap :: Map String (CompilerState -> CompilerState)
  stateIncMap = fromList [
    ("errorLabel",   \r -> r { errorLabel   = errorLabel   r + 1 }),
    ("handledLabel", \r -> r { handledLabel = handledLabel r + 1 }),
    ("typeVar",      \r -> r { typeVar      = typeVar      r + 1 }),
    ("variable",     \r -> r { variable     = variable     r + 1 }) ]

  incField :: CompilerState -> String -> CompilerState
  incField rec fieldName = (stateIncMap ! fieldName) rec


  getC :: MonadState CompilerState m => (CompilerState -> a) -> m a
  getC field = do
    s <- get
    return . field $ s

  incC :: MonadState CompilerState m => String -> m ()
  incC fieldName = do
    s <- get
    put $ s `incField` fieldName

  freshName :: MonadState CompilerState m => String -> m String
  freshName prefix = do
    n <- getC variable
    incC "variable"
    return $ prefix ++ '_' : show n

  freshTypeVar :: MonadState CompilerState m => m Type
  freshTypeVar = do
    n <- getC typeVar
    incC "typeVar"
    return $ Tvar $ 'a' : show n

  freshArg :: MonadState CompilerState m => Type -> m String
  freshArg t = freshName $ "_a_" ++ genId t

  genNames :: MonadState CompilerState m => [Type] -> m [String]
  genNames = mapM freshArg

  freshPMVar :: MonadState CompilerState m => Type -> m String
  freshPMVar t = freshName $ "_u_" ++ genId t

  freshWildcard :: MonadState CompilerState m => Type -> m String
  freshWildcard t = freshName $ "_w_" ++ genId t

  freshKVar :: MonadState CompilerState m => Type -> m String
  freshKVar t = freshName $ "_K_" ++ genId t

  freshLambda :: MonadState CompilerState m => Type -> m String
  freshLambda t = freshName $ "_Lam_" ++ genId t

  nextErrorLabel :: MonadState CompilerState m => m Label
  nextErrorLabel = do
    n <- getC errorLabel
    incC "errorLabel"
    return $ L $ "_error_" ++ show n

  nextHandledLabel :: MonadState CompilerState m => m Label
  nextHandledLabel = do
    n <- getC handledLabel
    incC "handledLabel"
    return $ L $ "_handled_" ++ show n
