{-# LANGUAGE
  FlexibleContexts
  #-}

module VMCode.Counters where
  import SPARC.Syntax
  import Types

  import Control.Monad.State

  data Counter = Cs {
    exceptionLabel :: Integer,
    identifier :: Integer
  }

  emptyState :: Counter
  emptyState = Cs 0 0

  nextId :: MonadState Counter m => String -> m String
  nextId s = do
    st <- get
    put st{ identifier = identifier st + 1 }
    return $ '_' : s ++ '_' : show (identifier st)

  nextExceptionLabel :: MonadState Counter m => m SPARC.Syntax.Label
  nextExceptionLabel = do
    st <- get
    put st{ exceptionLabel = exceptionLabel st + 1 }
    return $ SPARC.Syntax.L $ "exception_" ++ show (exceptionLabel st)

  seq :: MonadState Counter m => Instr -> Seq -> m Seq
  seq i e = do
    idf <- nextId "tmp"
    return $ Let idf Tunit i e
