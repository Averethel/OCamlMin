{-# LANGUAGE
  FlexibleContexts
  #-}

module AlphaConvert (alphaConvert, alphaConvertWithEnv) where
  import qualified AlphaConvert.AlphaConvert as AC
  import AlphaConvert.Env
  import CompilerState
  import KNormal.KSyntax

  import Control.Monad.State

  alphaConvert :: MonadState CompilerState m => KExpr -> m KExpr
  alphaConvert = alphaConvertWithEnv emptyEnv

  alphaConvertWithEnv :: MonadState CompilerState m => Env -> KExpr -> m KExpr
  alphaConvertWithEnv = AC.alphaConvert
