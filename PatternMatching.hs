{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching (compilePatternMatching) where
  import Counters
  import PatternMatching.EliminateLetSubpatterns
  import PatternMatching.NameWildcards
  import PatternMatching.NumbersToIfs
  import PatternMatching.ToHandles
  import PatternMatching.ToCases
  import PatternMatching.SimplifyHandles

  import Control.Monad.State

  import TypedSyntax

  compilePatternMatching :: MonadState Counter m => TypedExpr -> m TypedExpr
  compilePatternMatching e = do
    e1 <- nameWildcards e
    e2 <- eliminateLetSubPatterns e1
    e3 <- numbersToIfs e2
    e4 <- functionsToHandles e3
    e5 <- handlesToCases e4
    return $ simplifyHandles e5

