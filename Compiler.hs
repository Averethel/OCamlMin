module Compiler (compile) where
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
  import qualified SPARC.Syntax as S
  import Syntax
  import TypedSyntax
  import TypeInference
  import VMCode

  compiler :: Integer -> TypedExpr -> IO S.Program
  compiler t e0 = do
    let e1  = compilePatternMatching e0
    let e2  = convertToKNormal e1
    let e3  = alphaConvert e2
    e4     <- betaReduce e3
    let e5  = letFlatten e4
    e6     <- inline t e5
    let e7  = constantsFold e6
    e8     <- eliminateDefinitions e7
    e9     <- closureConvert e8
    let e10 = generateVMCode e9
    e11    <- optimizeProgram e10
    regAllocProgram e11

  compile :: Integer -> Expr -> IO (Either String S.Program)
  compile inlineTreshold expr = case typeOfExpression emptyEnv expr of
    Left er -> return $ Left er
    Right t -> do
      c <- compiler inlineTreshold t
      return $ Right c
