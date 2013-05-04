{-# LANGUAGE
  FlexibleContexts
  #-}

module AlphaConvert (alphaConvert, alphaConvertWithEnv) where
  import qualified AlphaConvert.AlphaConvert as AC
  import AlphaConvert.Env
  import Counters
  import KNormal.KSyntax

  import Control.Monad.State

  alphaConvert :: MonadState Counter m => KExpr -> m KExpr
  alphaConvert = alphaConvertWithEnv emptyEnv

  alphaConvertWithEnv :: MonadState Counter m => Env -> KExpr -> m KExpr
  alphaConvertWithEnv = AC.alphaConvert
