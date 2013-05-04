{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.UnaryPrim where
  import Counters
  import Syntax.UnaryPrim
  import Types

  import Control.Monad.State

  typeOfUnaryPrim :: MonadState Counter m => UnaryPrim -> m Type
  typeOfUnaryPrim UPnot   = return $ Tfun [Tbool] Tbool
  typeOfUnaryPrim UPref   = do
    v <- freshTypeVar
    return $ Tfun [v] $ Tref v
  typeOfUnaryPrim UPderef = do
    v <- freshTypeVar
    return $ Tfun [Tref v] v
  typeOfUnaryPrim UPminus = return $ Tfun [Tint] Tint
