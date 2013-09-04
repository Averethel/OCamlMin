{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler where
  import AlphaConvert
  import BetaReduce
  import ClosureConvert
  import ConstantsFold
  import EliminateDefinitions
  import Inline
  import Immidiate
  import KNormal
  import LetFlatten
  import PatternMatching
  import RegAlloc
  import X86.Syntax
  import Syntax
  import TypedSyntax
  import TypeInference
  import VMCode
  import Emit

  import CompilerState
  import Control.Monad.State

  compiler :: (MonadIO m, MonadState CompilerState m) =>
              Integer -> TypedExpr -> m Prog
  compiler t e0 = do
    e1     <- compilePatternMatching e0
    e2     <- convertToKNormal e1
    e3     <- alphaConvert e2
    e4     <- liftIO $ betaReduce e3
    let e5  = letFlatten e4
    e6     <- inline t e5
    let e7  = constantsFold e6
    e8     <- liftIO $ eliminateDefinitions e7
    e9     <- liftIO $ closureConvert e8
    e10    <- generateVMCode e9
    e11    <- liftIO $ optimizeProgram e10
    e12    <- regAllocProgram e11
    emitProgram e12

  compile :: (MonadIO m, MonadState CompilerState m) =>
             Integer -> Expr -> m (Either String Prog)
  compile inlineTreshold expr = do
    tp <- typeOfExpression emptyEnv expr
    case tp of
      Left er -> return $ Left er
      Right t -> do
        c <- compiler inlineTreshold t
        return $ Right c
