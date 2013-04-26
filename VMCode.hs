{-# LANGUAGE
  FlexibleContexts
  #-}

module VMCode (generateVMCode) where
  import qualified ClosureConvert.CSyntax as C
  import VMCode.Counters
  import VMCode.Generate
  import qualified SPARC.Syntax as S

  import Control.Monad.State

  generateVMCode :: C.Program -> S.Program
  generateVMCode p = fst $ runState (translateProgram p) emptyState
