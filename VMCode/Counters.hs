{-# LANGUAGE
  FlexibleContexts
  #-}

module VMCode.Counters where
  import SPARC.Syntax
  import Types

  import Control.Monad.State

  data Counter = Cs {
    errorLabel   :: Integer,
    handledLabel :: Integer,
    identifier   :: Integer
  }

  emptyState :: Counter
  emptyState = Cs 0 0 0

  nextId :: MonadState Counter m => String -> m String
  nextId s = do
    st <- get
    put st{ identifier = identifier st + 1 }
    return $ '_' : s ++ '_' : show (identifier st)

  nextErrorLabel :: MonadState Counter m => m Label
  nextErrorLabel = do
    st <- get
    put st{ errorLabel = errorLabel st + 1 }
    return $ L $ "_error_" ++ show (errorLabel st)

  nextHandledLabel :: MonadState Counter m => m Label
  nextHandledLabel = do
    st <- get
    put st{ handledLabel = handledLabel st + 1 }
    return $ L $ "_handled_" ++ show (handledLabel st)

  seq :: MonadState Counter m => Instr -> Seq -> m Seq
  seq i e = do
    idf <- nextId "tmp"
    return $ Let idf Tunit i e
