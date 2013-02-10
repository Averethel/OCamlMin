{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching (compilePatternMatching) where
  import PatternMatching.Counters
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
    e2 <- numbersToIfs e1
    e3 <- functionsToHandles e2
    e4 <- handlesToCases e3
    return $ simplifyHandles e4

  compilePatternMatching :: Expr -> Expr
  compilePatternMatching e = fst $ runState (matcherCompiler e) emptyState