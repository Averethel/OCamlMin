{-# LANGUAGE
  FlexibleContexts
  #-}

module VMCode (generateVMCode) where
  import Counters
  import qualified ClosureConvert.CSyntax as C
  import VMCode.Generate
  import qualified SPARC.Syntax as S

  import Control.Monad.State

  generateVMCode :: MonadState Counter m => C.Program -> m S.Program
  generateVMCode = translateProgram
