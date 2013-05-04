{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.BinaryPrim where
  import Counters
  import Syntax.BinaryPrim
  import Types

  import Control.Monad.State

  typeOfBinaryPrim :: MonadState Counter m => BinaryPrim -> m Type
  typeOfBinaryPrim BPeq     = do
    v <- freshTypeVar
    return $ Tfun [v, v] Tbool
  typeOfBinaryPrim BPlt     = return $ Tfun [Tint, Tint] Tbool
  typeOfBinaryPrim BPgt     = return $ Tfun [Tint, Tint] Tbool
  typeOfBinaryPrim BPor     = return $ Tfun [Tbool, Tbool] Tbool
  typeOfBinaryPrim BPand    = return $ Tfun [Tbool, Tbool] Tbool
  typeOfBinaryPrim BPadd    = return $ Tfun [Tint, Tint] Tint
  typeOfBinaryPrim BPsub    = return $ Tfun [Tint, Tint] Tint
  typeOfBinaryPrim BPmult   = return $ Tfun [Tint, Tint] Tint
  typeOfBinaryPrim BPdiv    = return $ Tfun [Tint, Tint] Tint
  typeOfBinaryPrim BPmod    = return $ Tfun [Tint, Tint] Tint
  typeOfBinaryPrim BPassign = do
    v <- freshTypeVar
    return $ Tfun [Tref v, v] Tunit
