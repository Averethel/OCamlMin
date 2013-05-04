{-# LANGUAGE
  FlexibleContexts
  #-}

module Inline (inline, defaultTreshold) where
  import Inline.Env
  import qualified Inline.Inline as I

  import KNormal.KSyntax

  import Counters
  import Control.Monad.State

  defaultTreshold :: Integer
  defaultTreshold = 0

  inline :: (MonadIO m, MonadState Counter m) => Integer -> KExpr -> m KExpr
  inline = I.inline emptyEnv
