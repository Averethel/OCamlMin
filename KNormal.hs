{-# LANGUAGE
  FlexibleContexts
  #-}

module KNormal (
  convertToKNormal,
  KExpr(..)
) where
  import CompilerState
  import KNormal.KNormalize
  import KNormal.KSyntax

  import TypedSyntax

  import Control.Monad.State

  convertToKNormal :: MonadState CompilerState m => TypedExpr -> m KExpr
  convertToKNormal = kNormalize
