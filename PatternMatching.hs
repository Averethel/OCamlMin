{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching (compilePatternMatching) where
  import PatternMatching.Counters
  import PatternMatching.EliminateLetSubpatterns
  import PatternMatching.NameWildcards
  import PatternMatching.NumbersToIfs
  import PatternMatching.ToCases
  import PatternMatching.ToHandles
  import PatternMatching.SimplifyHandles

  import Control.Monad.State

  import Syntax

  matcherCompiler :: MonadState Counter m => Expr -> m Expr
  matcherCompiler e = do
    e1 <- nameWildcards e
    e2 <- eliminateLetSubPatterns e1
    e3 <- numbersToIfs e2
    e4 <- functionsToHandles e3
    e5 <- handlesToCases e4
    return $ simplifyHandles e5

  compilePatternMatching :: Expr -> Expr
  compilePatternMatching e = fst $ runState (matcherCompiler e) emptyState
