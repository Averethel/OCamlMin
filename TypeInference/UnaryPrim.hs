{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.UnaryPrim where
  import Syntax.UnaryPrim
  import Types

  import TypeInference.Counter

  import Control.Monad.State

  typeOfUnaryPrim :: MonadState Counter m => UnaryPrim -> m Type
  typeOfUnaryPrim UPnot   = return $ Tfun [Tbool] Tbool
  typeOfUnaryPrim UPref   = do
    v <- freshVar
    return $ Tfun [v] $ Tref v
  typeOfUnaryPrim UPderef = do
    v <- freshVar
    return $ Tfun [Tref v] v
  typeOfUnaryPrim UPminus = return $ Tfun [Tint] Tint
