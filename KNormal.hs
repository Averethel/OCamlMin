{-# LANGUAGE
  FlexibleContexts
  #-}

module KNormal (
  convertToKNormal,
  KExpr(..)
) where
  import Counters
  import KNormal.KNormalize
  import KNormal.KSyntax

  import TypedSyntax

  import Control.Monad.State

  convertToKNormal :: MonadState Counter m => TypedExpr -> m KExpr
  convertToKNormal = kNormalize
